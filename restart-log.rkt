#lang racket


; security feature
; in charge of logging restart commands


(require
  racket/date
  "boom-parameters.rkt"
  "misc.rkt"
  "restart-log-loader.rkt"
  "restart-log-msg.rkt")


; builds diary record
; exe-path should be the path to the executable, that is called by "restart" command.
; if this argument is empty, it is replaced with an error message, that will be output to the diary
(define/contract (make-record exe-path)
  (-> string? string?)

  (let ([current-date-format (date-display-format)] ; backs up date display format
        [result ((λ(arg)
                  (date-display-format restart-log-date)
                 
                  (~a "["
                      (now->string #f)
                      "] "
                      (me-or non-empty-string? arg exe-missing))) exe-path)])

    (date-display-format current-date-format) ; restores date-display format
    result))


; outputs a single record to restart calls diary
; or nothing if records is an empty string
(define/contract (write-record r out)
  (-> string? port? void?)

  (when (non-empty-string? r)
    (display r out)
    (newline out)))


; outputs a list of records to restart calls diary
(define/contract (write-batch record-list out)
  (-> list?
      port?
      boolean?)

  (if (empty? record-list)
      #t
      (andmap (λ(r)
                (with-handlers ([exn:fail? (λ(e) #f)])
                  (write-record r out)
                  #t))
              record-list)))


; Takes an executable file name, includes it into a record and outputs the record to restart calls diary
(define/contract (write-file-name exe-name out)
  (-> string? port? boolean?)
  
  (write-record
   (make-record exe-name)
   out)
  #t)


(struct record-data(target content primitive formating)
  #:guard (λ(t c p f name)
            (when (neither (string? t)
                           (path? t))
              (raise-argument-error "target"
                                    "string? or path?"
                                    1
                                    t))

            (when (neither
                   (string? c)
                   (list? c))
              (raise-argument-error "content"
                                    "string?"
                                    2
                                    c))

            (when (neither (and (string? c)
                                (eq? write-file-name p))
                           (and (list? c)
                                (eq? write-batch p)))
              (raise-argument-error "primitive"
                                    "(or/c write-bach write-file-name)"
                                    3
                                    p))
            
            (when (not-in-list f
                               (list 'raw 'formated))
              (raise-argument-error "formating"
                                    "(list 'raw 'formated)"
                                    4
                                    f))

            (values t c p f)))


; implements basic protocole for accessing diary
; writing primitive can either write-file-name or write-batch
(define/contract (protocole-with-primitive info)
  (-> record-data?
      boolean?)

  (with-handlers ([exn:fail? (λ(e) #f)])
    (call-with-output-file
        (record-data-target info)
      (λ(out)
        ((record-data-primitive info)
         (record-data-content info)
         out)
        #t)
      #:mode 'text
      #:exists 'replace)))


; function to be used once diary has been created
(define (append-logs info)
  (with-handlers ([exn:fail? (λ(e) #f)])
    (let* ([previous-content (load-rsl
                              (record-data-target info))]
           [new-content (append previous-content
                                (list
                                 (make-record (record-data-content info))))]
           [new-info (record-data (record-data-target info)
                                  new-content
                                  write-batch
                                  'raw)])

    
      (protocole-with-primitive new-info))))


(define/contract (record-restart diary restart-command)
  (-> (or/c string? path?)
      string?
      boolean?)
  
  ((if (file-exists? diary)
       append-logs
       protocole-with-primitive) (record-data diary
                                              restart-command
                                              write-file-name
                                              'raw)))
