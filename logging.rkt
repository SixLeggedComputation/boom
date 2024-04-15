#lang racket

; module in charge of sending information about program execution
; It is better not to include any module from the application into this one
; In most cases texts are not localized, because they're intended for developpers, not users

(require "misc.rkt"
         racket/logging
         "logstrings.rkt")


(define space-str
  (make-string 1 #\space))


(define/contract (connect-sentences start follow)
  (-> (or/c non-empty-string? void?)
      non-empty-string?
      string?)

  (let* ([base-sentence-sep (string-join
                             (list "." space-str)
                             "")]
         [list1 (list "" space-str base-sentence-sep)]
         [list2 (list "" "." base-sentence-sep)]
         [cur-sentence-sep (let ([end-match ((λ(first-term)
                                               (if (void? first-term)
                                                   2
                                                   (let ([sl (string-length first-term)])
                                                     (cond
                                                       [(string-ci=? space-str first-term) 0]
                                                       [(last-char? first-term #\.) 1]
                                                       [(string-suffix? first-term base-sentence-sep) 0]
                                                       [else 2])))) start)]
                                 
                                 [begin-match ((λ(last-term)
                                                 (let ([first-char (substring last-term 0 1)])
                                                   (cond
                                                     [(string-ci=? first-char space-str) 1]
                                                     [(string-ci=? "." first-char) 0]
                                                     [else 2]))) follow)])
                             
                             
                             (cond
                               [(= 0 end-match) (car list1)]
                               [(and
                                 (= 1 end-match)
                                 (= 0 begin-match)) (car list1)]
                               [(and
                                 (= 1 end-match)
                                 [= 1 begin-match]) (list-ref list1 end-match)]
                               [(and
                                 (= 1 end-match)
                                 (= 2 begin-match)) (list-ref list1 end-match)]
                               [(and
                                 (= 2 end-match)
                                 (= 0 begin-match)) (car list1)]
                               [(and
                                 (= 2 end-match)
                                 (= 1 begin-match)) (list-ref list2 begin-match)]
                               [else base-sentence-sep]))]
         [trailer (string-append cur-sentence-sep follow)])
    (if (void? start)
        trailer
        (string-append start trailer))))


(define (test-cs1)
  (connect-sentences "a" "b"))

(define (test-cs2)
  (connect-sentences "a." "b"))


(define (test-cs3)
  (connect-sentences (void) "No data provided"))


(define/contract (connect-words w-before w-after)
  (-> string? string? string?)
  
  (string-normalize-spaces
   (string-join
    (list w-before w-after))))
      


(define/contract (msg-code-issue [issue-data (void)])
  (->* ()
       (any/c)
       string?)
  
  (let ([seed "internal error"])
    (if (void? issue-data)
        (connect-sentences seed "No data provided")
        (connect-words seed
                       (format "with data: ~a"
                               issue-data)))))

(define (test-mci1)
  (msg-code-issue (void)))

(define (test-mci2)
  (msg-code-issue "could not attach controls"))

(define/contract (check-emptiness v)
  (-> string? string?)
  
  (if (non-empty-string? v)
      v
      empty-msg-par))

(define (msg-invalid-url command-line-value)
  (let ([translated-par (check-emptiness
                         (~a command-line-value))])
    (format "Crash report ~a could not be loaded. Either its location or its file type are invalid"
            translated-par)))

(define boom-logger
  (make-logger
   'boom
   (current-logger)))


(define (log-custom severity text [data #f])
  ;(->* (log-level/c string?)
  ;     any/c)
  
  (log-message boom-logger
               severity
               #f
               text
               data
               logger-prefix))


(define/contract (log-err text [data #f])
  (->* (string?)
       any/c)
  
  (log-custom 'error text data))


(define (warn text [data #f])
  (log-custom 'warning text data))


; general method, that is to be used with with-handlers
; ex = the exception, that was caught
; lethal? = should this exception be logged as error or warning?
(define (log-general ex [lethal? #t])
  (let-values ([(proc qualifier) (if lethal?
                                     (values log-err msg-lethal)
                                     (values warn msg-non-lethal))])
    (proc
     (format msg-exception-caught
             qualifier
             ex)
     ex)))


; to be used when code is broken
(define (log-code-issue [issue-data #f])
  (log-err (msg-code-issue (if (boolean? issue-data)
                               (if (not issue-data)
                                   (void)
                                   issue-data)
                               issue-data))
           issue-data))


; general member, that is to be called by any function, which must format a value into an error message
(define/contract (log-invalid-value mask-generator value)
  (-> procedure? any/c void?)
  
  (log-err (mask-generator value)
           value))


(define (log-invalid-icon location)
  (log-invalid-value msg-invalid-bitmap location))


; invalid value for prompt font size
(define (log-invalid-font-size value)
  (log-invalid-value msg-invalid-font-size value))


; unrecognized prompt font face
(define (log-invalid-font-face value)
  (log-invalid-value msg-invalid-face value))


(define (log-invalid-location loc)
  (log-err (msg-invalid-url loc)
           loc))

(define (log-no-report loc)
  (log-err  msg-no-report loc))


; Warns about incorrect friendly-text-mask value in configuration file
(define/contract (warn-ftm should-be-set?)
  (-> boolean? void?)
  
  (warn (if should-be-set?
            msg-ftm-set
            msg-ftm-unset)))


(define (warn-ft-config)
  (warn msg-ft-warning))


(define (warn-config-exception e)
  (log-general e #f))


(define (warn-invalid-id cause)
  (warn
   (if (non-empty-string? cause)
       (format msg-wrong-id
               cause)
       msg-empty-id)))


(provide
 connect-sentences
 log-code-issue
 log-general
 log-invalid-icon
 log-invalid-font-face
 log-invalid-font-size
 log-invalid-location
 log-no-report
 warn-ftm
 warn-ft-config
 warn-config-exception
 warn-invalid-id)
