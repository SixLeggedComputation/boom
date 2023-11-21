#lang racket

; window can be cnfigured either by passing parameters in command line or by a config file
; when no report is found, action fields are disabled.


(require json
         racket/gui
         net/http-client
         "strings-manager.rkt"
         "replacements.rkt"
         "boom-windows.rkt"
         "controls.rkt"
         "boom-parameters.rkt"
         "boom-actions.rkt"
         "args.rkt"
         "fakerep.rkt"
         "args.rkt"
         "text-processing.rkt")


(define reporting-tool-background-color
  (make-color 200 200 200 0.8))
(define bug-tracker "my-bug-tracker")


; Global variable that holds report details window current status
; needed with discovery-button%
(define report-showing #t)


; loads report and turns its json content into hasheq
; file location is searched in command line parameters. If none is provided, report is searched in app directory.
; if no report at all, a string constant is loaded, which mimics a report and will inform user about this faulty condition
(define crash-data
  (with-handlers ([exn:fail? (λ(e) (substitute-report))])
    (with-input-from-string
        (call-with-input-file
            (report-file-location)
          (λ(in) (read-line in)))
      (λ() (read-json)))))


(define report-display
  (hash-ref
   crash-data
   'description
   replace-report))


(define (has-action? action-key more-checks)
  (if (hash-has-key? crash-data action-key)
      (let ([value (hash-ref
                    crash-data
                    action-key
                    '())])
        (if (string? value)
            (if (procedure? more-checks)
                (more-checks value)
                #t)
            #t))
      #f))           


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

  (let* ([formatted-text (clip-text canvas report-display '(5 5))]
         [fs (ceiling
              (send
               (send dc get-font)
               get-size
               #t))] ; gets current font height as an integer
         [ylocations (λ(nb-lines)
                       (map
                        (λ(n) (+ (* n fs) 5))
                        (range nb-lines)))] ; computes text lines y coordinates. nb-lines is number of lines obtained from clipping procedure
         [display-line (λ(x y s)
                         (send dc draw-text s x y 'grapheme))]) ; primitive function for displaying a string to canvas

    (if (list? (clipped-text formatted-text))
        (let ([par-lines (length (clipped-text formatted-text))])
          (cond
            [(= 0 par-lines)
             (display-line 5 5 "Error")]
            [(= 1 par-lines)
             (display-line 5 5 (car (clipped-text formatted-text)))]

            [else
             (map (λ(s y)
                    (display-line 5 y s)
                    #t)
                  (clipped-text formatted-text)
                  (ylocations par-lines))]))
        
        (display-line 5 5 report-display))))


(define report-window
  (let ([c (new canvas%
                [parent report-panel]
                [style (list 'control-border 'vscroll)]
                [paint-callback paint-report])])
    (send c set-canvas-background reporting-tool-background-color)
    c))

(set-field! companion report-button report-window)


(define (display-actions)
  (let([tracking-allowed? (has-action? 'tracker '())]
       [restart-allowed? (has-action? 'restart '(λ(f) (file-exists? f)))])
    (when (or tracking-allowed? restart-allowed?)
      (make-actions boom-window
                    (enabled-field tracking-allowed? restart-allowed?)))))


(define action-control
  (display-actions))


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
