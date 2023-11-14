#lang racket

(require racket/gui)


(define (make-close-button caption container form)
  (new button%
       [label caption]
       [parent container]
       [callback (λ(button event)
                   (send form show #f))]))

(provide make-close-button)
  