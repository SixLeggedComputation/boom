#lang racket

; basic controls which are derived from racket/gui classes

(require racket/gui)


(define (make-close-button caption container form [action-handler (void)])
  (new button%
       [label caption]
       [parent container]
       [callback (λ(button event)
                   
                   (when (procedure? action-handler)
                     (action-handler))
                   
                   (send form show #f))]))

(provide make-close-button)
  