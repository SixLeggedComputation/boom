#lang racket

; defines string constants and masks, that are used for logging.
; these values are not localized, which is why they are defined apart from constants involved in user dialogs
; this file is intended to be included only by logging.rkt

(define logger-prefix "boom")
(define msg-exception-caught "A ~a exception was raised: ~a")
(define msg-no-report "Caller sent no report or report could not be read")
(define msg-ftm-set "friendly text mask flag should be set")
(define msg-ftm-unset "Check friendly text placeholder or unset mask flag")
(define msg-ft-warning "friendly expression not evaluated")
(define msg-lethal "lethal")
(define msg-non-lethal "non lethal")
(define empty-msg-par "[!Empty]")


(define (msg-invalid-face value)
  (format "~a is not a valid font face" value))


(define (msg-invalid-font-size value)
  (format "~a is no valid prompt font size" value))


(define (msg-invalid-bitmap location)
  (format "~a either is not a valid location or does not point to a valid icon file"
          (if (void? location)
              empty-msg-par
              location)))

(provide
 (all-defined-out))