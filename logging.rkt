#lang racket

(define logger-prefix "boom")
(define msg-exception-caught "An exception was raised")
(define msg-no-report "Caller sent no report or report could not be read")
(define empty-msg-par "[!Empty]")


(define (msg-invalid-bitmap location)
  (format "~a either is not a valid location or does not point to a valid icon file"
          (if (void? location)
              empty-msg-par
              location)))

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


(define (log-err text data)
  (log-message boom-logger
               'error
               #f
               text
               data
               logger-prefix))


(define (log-general ex)
  (log-err msg-exception-caught ex))


(define (log-invalid-icon location)
  (log-err (msg-invalid-bitmap location)
           location))


(define (log-invalid-location loc)
  (log-err (msg-invalid-url loc)
           loc))

(define (log-no-report loc)
  (log-err  msg-no-report loc))


(provide
 log-general
 log-invalid-icon
 log-invalid-location
 log-no-report)
