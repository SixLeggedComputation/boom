#lang racket

; programs args:
; -r --report report file location
; -t --tracker bug tracker url

(require json
         racket/gui
         net/http-client
         "strings-manager.rkt"
         "replacements.rkt"
         "boom-windows.rkt"
         "controls.rkt"
         "boom-parameters.rkt")


(define reporting-tool-background-color
  (make-color 200 200 200 0.8))
(define bug-tracker "my-bug-tracker")
(define default-report-file "crash.json")

; Global variable that holds report details window current status
; needed with discovery-button%
(define report-showing #t)


(define enable-tracking #t)


(define enable-restart #t)


(define crash-data
  (with-input-from-string
      (call-with-input-file "crash.json"
        (λ(in) (read-line in)))
    (λ() (read-json))))


(define report-display
  (hash-ref
   crash-data
   'description
   replace-report))


(define header-font%
  (make-object font% 12.0 'modern ))

(define message-font
  (make-object
      font%
    10.0
    'modern
    'normal
    'semilight))


(define boom-window
  (new frame%
       [label (hash-ref crash-data 'software (rstr 'cdnf replace-cdnf))]
       [min-height 800]
       [min-width 200]
       [stretchable-height 1000]
       [style (list 'float)]))


(define header-panel
  (new horizontal-panel%
       [parent boom-window]
       [horiz-margin default-spacing]
       [vert-margin default-spacing]))


(define icon-canvas
  (let* ([icon (read-bitmap "/home/ml/Documents/boom/boom2.png")])
    (let-values ([(icon-width icon-height) (values (send icon get-width) (send icon get-height))])
      (new canvas%
           [parent header-panel]
           [min-height icon-height]
           [min-width icon-width]
           [paint-callback (λ(canvas dc)
                             (send dc draw-bitmap icon 0 0))]))))

(define header
  (new message% [parent header-panel]
       [label (~a "Sorry, "
                  (hash-ref crash-data 'software (rstr 'smip replace-smip))
                  (rstr 'cpbody replace-cpbody))]
       [font header-font%]
       [horiz-margin default-spacing]))


(define report-panel
  (new vertical-panel%
       [parent boom-window]
       [horiz-margin default-spacing]))


(define (get-hide-caption)
  (rstr 'rblh replace-rblh))


(define discovery-button%
  (class button%
    (field [companion (void)])

    (define (capture-click button event)
      (when (not (void? companion))
        (if report-showing
            (begin
              (set! report-showing #f)
              (send this set-label (rstr 'rbls replace-rbls)))
            (begin
              (set! report-showing #t)
              (send this set-label (get-hide-caption))))
        (send companion show report-showing)))
    
    (super-new [callback capture-click]
               [vert-margin default-spacing])))

(define report-button
  (new discovery-button%
       [parent report-panel]
       [label (get-hide-caption)]))


(define (paint-report canvas dc)
  (send dc clear)
  (send dc set-text-foreground "red")
  (send dc draw-text report-display 5 5))


(define report-window
  (let ([c (new canvas%
                [parent report-panel]
                [style (list 'control-border 'vscroll)]
                [paint-callback paint-report])])
    (send c set-canvas-background reporting-tool-background-color)
    c))

(set-field! companion report-button report-window)


(define action-panel
  (new group-box-panel%
       [parent boom-window]
       [label (rstr 'actbox replace-actbox)]
       [alignment '(left center)]
       [vert-margin default-spacing]
       [horiz-margin default-spacing]))


(define action-layout
  (new horizontal-panel%
       [parent action-panel]
       [alignment '(center center)]))


(define check-send
  (new check-box%
       [parent action-layout]
       [label (rstr 'capsend replace-capsend)]
       [value #f]
       [horiz-margin default-spacing]))


(define check-restart
  (new check-box%
       [parent action-layout]
       [label (rstr 'caprestart replace-restart)]
       [value #t]
       [horiz-margin default-spacing]))


(define action-help
  (new message%
       [parent action-panel]
       [label (rstr 'acthelp replace-acthelp)]
       [horiz-margin default-spacing]))


(define (send-report conn content)
  (http-conn-send!
   conn
   bug-tracker
   #:method "POST"
   #:data content
   #:headers (list "Content-Type: application/x-www-form-urlencoded")))
    

(define command-panel
  (new horizontal-panel%
       [parent boom-window]
       [alignment '(center center)]
       [vert-margin default-spacing]))


(define exit-button
  (make-close-button (rstr 'capexit replace-capexit)
                     command-panel
                     boom-window))


(define config-dialog
  (make-config-dialog boom-window))


(send boom-window center)
(send boom-window show #t)

(when alert-locale-error
  (send config-dialog show #t))
