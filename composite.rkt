#lang racket

; controls which are derived from racket/gui and composed with other controls or derived controls

(require racket/gui
         "boom-parameters.rkt"
         "strings-manager.rkt"
         "replacements.rkt")


(struct enabled-field (send restart))

(define actions%
  (class group-box-panel%
    (init-field[action-parameters (enabled-field #t #t)])
               
    (super-new [label (rstr 'actbox replace-actbox)]
               [alignment '(left center)]
               [vert-margin default-spacing]
               [horiz-margin default-spacing])

    (define boxes-layout
      (new horizontal-panel%
           [parent this]
           [alignment '(center center)]))

    (define/private (make-check-send)
      (when (enabled-field-send action-parameters)
        (new check-box%
             [parent boxes-layout]
             [label (rstr 'capsend replace-capsend)]
             [value #f]
             [horiz-margin default-spacing])))

    (define check-send (make-check-send))

    (define/private (make-check-restart)
      (new check-box%
           [parent boxes-layout]
           [label (rstr 'caprestart replace-restart)]
           [value #t]
           [horiz-margin default-spacing]))

    (define check-restart (make-check-restart))


    (define action-help
      (new message%
           [parent this]
           [label (rstr 'acthelp replace-acthelp)]
           [horiz-margin default-spacing]))))


(define (make-actions container)
  (new actions% [parent container]))


(provide
 make-actions
 (struct-out enabled-field))