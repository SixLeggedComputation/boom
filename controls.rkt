#lang racket

; basic controls which are derived from racket/gui classes

(require racket/gui)


(define (make-close-button caption container form)
  (new button%
       [label caption]
       [parent container]
       [callback (λ(button event)
                   (send form show #f))]))

(provide make-close-button)
  