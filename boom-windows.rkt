#lang racket

(require racket/gui
         "controls.rkt"
         "boom-parameters.rkt")

(define config-error-msg
  "Configuration files are missing.\nCrash manager will display in American English")

(define (make-cmdlg-prompt text window)
  (new message%
       [parent window]
       [label text]
       [vert-margin default-spacing]
       [horiz-margin default-spacing]))

(define crash-manager-dialog%
  (class dialog%
    (init-field [prompt (void)])
    (super-new [label product-name])
    (make-cmdlg-prompt prompt this)
    (make-close-button "OK" this this)))


; creates a dialog to be used when config file is not found
(define (make-config-dialog main-window)
  (new crash-manager-dialog%
       [parent main-window]
       [prompt config-error-msg]))


(provide make-config-dialog)