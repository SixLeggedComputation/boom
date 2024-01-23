#lang racket

(define logger-prefix "boom")
(define msg-exception-caught "An exception was raised")
(define msg-no-report "Caller sent no report or report could not be read")

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


(define (log-no-report loc)
  (log-err  msg-no-report loc))


(provide
 log-general
 log-no-report)
