#lang racket

; window can be configured either by passing parameters in command line or by a config file
; when no report is found, action fields are disabled.
; actions to be undertaken when user clicks close button are listed inside exit-actions-handler callback

; method for adding an acion parameter in a way, that accounts for precedence order of the various value sources,
; which is hard-coded default < config file < command line < report file:
; 1 - if the action is to be displayed in user interface, then create corresponding field in struct enabled-field in boom-actions module
; 2 - define its hard-coded default value in module boom-parameters. Call it default-k, where k is parameter's name
; 3 - create an entry in config.txt. Call it cfg-k
; 4 - define-config-param accessor with name cfg-k in boom-config. Its default value must be default-k
; 5 - in args module create a command line accessor k. Its default value must be cfg-k
; 6 - change actions% init-field declaration in boom-actions so that its default value is the command line accessor

; the command line accessor will be the parmater initial value when setting up action% control.
; if action% is showing, this initial value can then be overriden by report file and user.
; Otherwise, this initial value is the one in use for the rest of session.

; modules define a debug-module field, which can be toggled, so as to debug particular modules. Its
; default value is controlled by debugger-on, which is defined in boom-parameters.


(require json
         racket/date
         racket/gui
         net/http-client
         "strings-manager.rkt"
         "replacements.rkt"
         "boom-windows.rkt"
         "controls.rkt"
         "boom-parameters.rkt"
         "actions-handling.rkt"
         "args.rkt"
         "fakerep.rkt"
         "text-processing.rkt"
         "time-reader.rkt"
         "time-display.rkt"
         "os-reader.rkt"
         "boom-actions.rkt"
         "starter.rkt"
         "accomplishment.rkt"
         "final-window.rkt"
         "logging.rkt")


(date-display-format (ddisp-style current-date-system))

(define reporting-tool-background-color
  (make-color 200 200 200 0.8))

(define bug-tracker "my-bug-tracker")


; Global variable that holds report details window current status
; needed with discovery-button%
(define report-showing #t)


(struct report-buffer(reading substitution))


; loads report and turns its json content into hasheq
; file location is searched in command line parameters. If none is provided, report is searched in app directory.
; if no report at all, a string constant is loaded, which mimics a report and will inform user about this faulty condition
(define crash-data
  (let* ([buid-unreadable-message (λ()
                                   (report-buffer (substitute-report)
                                                  #t))]
         [handle-failed-download (λ()
                                   (log-invalid-location report-raw-url)
                                   (buid-unreadable-message))])
    
    (if (send report-status anything-to-download?)
        
        (with-handlers ([exn:fail? (λ(e)
                                     (log-no-report (report-file-location))
                                     (buid-unreadable-message))])
          (report-buffer (with-input-from-string
                             (call-with-input-file
                                 (report-file-location)
                               (λ(in) (read-line in)))
                           (λ() (read-json)))
                         #f))
                
        (handle-failed-download))))


(define/contract (field-content which)
  (-> symbol? any/c)
  
  (hash-ref (report-buffer-reading crash-data)
            which
            #f))  


; extracts bug description from report
(define report-about
  (let* ([process (λ(content)
                    (if (or (string? content)
                            (boolean? content))
                        content
                        (~a content)))]
         [imported-data (process
                         (field-content 'description))]
         [validated (λ(content)
                      (if (string? content)
                          (non-empty-string? content)
                          #f))])
    (if (validated imported-data)
        imported-data
        replace-report)))


; extracts bug time stamp
(define report-time
  (read-report-time
   (report-buffer-reading crash-data)))


; extract caller software name and version for display
(define (caller-name)
  (let ([report-software (field-content 'software)]
        [report-restart (let ([p (field-content 'restart)])
                          (if (non-empty-string? p)
                              ; file-name-from-path returns anything after last dir separator. arguments must be trimmed off.
                              (file-name-from-path
                               (car
                                (string-split p))) ; string-split gets rid of possible command line arguments
                              #f))]
        [caller-version (field-content 'version)])
    (cond
      [(non-empty-string? report-software) (if (non-empty-string? caller-version)
                                               (string-join
                                                (list report-software caller-version))
                                               report-software)]
      [(and (boolean? report-software)
            (non-empty-string? report-restart)) report-restart]
      [else (rstr 'smip replace-smip)])))


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
  (new dialog%
       [label (hash-ref (report-buffer-reading crash-data)
                        'software
                        (rstr 'cdnf replace-cdnf))]
       [parent #f]
       [style (list 'no-caption)]))


(define header-panel
  (new horizontal-panel%
       [parent boom-window]
       [horiz-margin default-spacing]
       [vert-margin default-spacing]))


(define icon-canvas
  (let* ([icon (read-bitmap (main-icon))])
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
                  (caller-name)
                  (rstr 'cpbody replace-cpbody))]
       [font header-font%]
       [horiz-margin default-spacing]
       [auto-resize #t]))


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

  (let* ([formatted-text (clip-text canvas report-about '(5 5))]
         [fs (ceiling
              (send
               (send dc get-font)
               get-size
               #t))] ; gets current font height as an integer
         [pix-spacing (ceiling
                       (* default-line-spacing fs))]
         [ylocations (λ(nb-lines)
                       (map
                        (λ(n) (+ (* n (+ pix-spacing fs)) 5))
                        (range nb-lines)))] ; computes text lines y coordinates. nb-lines is number of lines obtained from clipping procedure
         [display-line (λ(x y s)
                         (send dc draw-text s x y 'grapheme))] ; primitive function for displaying a string to some canvas position
         [display-one (λ(s)
                        (display-line 5 5 s))])                ; shortcut to be used there is just one string to be displayed: prints string to top location 

    (if (list? (clipped-text formatted-text)) ; if clipping sucessed, text is returned as a list of strings, even when there is only 1 line
        (let ([par-lines (length (clipped-text formatted-text))])
          (cond
            [(= 0 par-lines)
             (display-one (rstr 'msgparser replace-msg-parser))]
            [(= 1 par-lines)
             (display-one (car (clipped-text formatted-text)))]

            [else
             (map (λ(s y)
                    (display-line (clipped-left formatted-text) y s)
                    #t)
                  (clipped-text formatted-text)
                  (ylocations par-lines))]))
        
        (display-one report-about))))


(define report-fields-panel
  (new vertical-panel%
       [parent report-panel]
       [spacing 0]
       [stretchable-height #f]))


(set-field! companion report-button report-fields-panel)


(define report-window
  (let ([c (new canvas%
                [parent report-fields-panel]
                [style (list 'control-border 'vscroll)]
                [paint-callback paint-report]
                [vert-margin 0])])
    (send c set-canvas-background reporting-tool-background-color)
    c))


(define meta-panel
  (new horizontal-panel%
       [parent report-panel]
       [spacing 0]
       [vert-margin 0]))

(define time-field
  (make-time-display meta-panel))


(define os-field
  (new message%
       [parent meta-panel]
       [label (~a "operating system: "
                  (os-reading-value
                   (read-report-os (report-buffer-reading crash-data))))]))

(send meta-panel min-height (send time-field get-height))


(define action-control
  (display-actions boom-window
                   (report-buffer-reading crash-data)))


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



(define (messenger content [buttons (list 'ok)])
  (message-box
   (rstr 'default-title
         replace-default-title)
   content
   boom-window
   buttons))


; the file should not be removed in following cases:
; - the window is showing the fake file
; - the removal option is disabled in config file
; - the removal option is enabled in config file but disabled by command line parameters
; - removal option disabled in interface
(define/contract (remove-report)
  (-> task-outcome?)
  
  (let ([eval-removal (λ() (cond ; conditions sorted according to precedence order
                             [(report-buffer-substitution crash-data) #f] ; the original crash report could not be read and what is displayed in UI is a placeholder
                             [(is-a? action-control actions%) (send action-control clear-file?)] ; looks for user check mark if actions interface is defined
                             [else (destroy-after-use)]))]) ; either crach report was not read or it did not specify any action, then look at default settings
    (with-handlers ([exn:fail:filesystem? (λ(e)
                                            (messenger
                                             (rstr 'cleaning-error
                                                   replace-cleaning-error))
                                            'failed)])
      (if (eval-removal)
          (begin
            (delete-file
             (report-file-location))
            'completed)
          'not-selected))))



(define/contract (exec-app)
  (-> task-outcome?)

  (let ([restart? (λ()
                    (cond
                      [(report-buffer-substitution crash-data) #f]
                      [(is-a? action-control actions%) (send action-control restart?)]
                      [else #f]))])
    
    (if (restart?)                     
        (let ([parent-app (extract-caller
                           (report-buffer-reading crash-data))]
              [alert-invalid (λ(cause)
                               (messenger
                                (format "App did not start due to incorrect command specification: ~a" cause))
                               'failed)]
              [starter-messenger (λ(content)
                                   (messenger content))]
              [starter-prompter (λ(content)
                                  (messenger content
                                             (list 'yes-no)))])
          
          (case (caller-status parent-app)
            ['nf (alert-invalid "executable not found")]
            ['nr (alert-invalid "no valid executable in report")]
            ['new (alert-invalid "not an executable")]
            ['stx (alert-invalid "error in statement")]
            ['fine (begin
                     (start-caller parent-app
                                   starter-messenger
                                   starter-prompter)
                     'completed)]))

        'not-selected)))


(define final-state
  (let ((result (make-hasheq)))
    (hash-set! result  'send 'not-selected)
    (hash-set! result 'cleanup 'not-selected)
    (hash-set! result 'restart 'not-selected)
    result))


; callback, that is executed when boom window close button is hit
; add additional action handlers after or before remove-report
(define exit-actions-handler
  (λ ()
    (hash-set! final-state 'cleanup (remove-report))
    (hash-set! final-state 'restart (exec-app))))


(define exit-button
  (make-close-button (rstr 'capexit replace-capexit)
                     command-panel
                     boom-window
                     exit-actions-handler))


(define config-dialog
  ; no parent because this dialog pops up before main form in case of error
  (make-config-dialog #f))


(with-handlers ([exn:fail? (λ(e)
                             (log-general e))])
  (when alert-locale-error
    (send config-dialog show #t))

  (send boom-window center)
  (send boom-window show #t)

  (when (show-summary)
    (show-end boom-window final-state)

    (when (summary-status-changed discarded)
      (edit-config
       "show-summary"
       (not discarded)))))