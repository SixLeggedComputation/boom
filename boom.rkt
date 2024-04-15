#lang racket

; A popup window, which can be configured either by passing parameters through command line or by a config file.
; It is divided into 3 sections: prompt, report, actins
; Prompt informs user about caller crash.
; Report displays report data
; Actions lists some actions to be undertaken when user clicks on close button
; actions are listed inside exit-actions-handler callback
; when no report is found, action fields are disabled.

; Steps for adding an action parameter, while accounting for configuration modifiers precedence order.
; This order is: hard-coded default < config file < command line < report file:
; 1 - if the action is to be displayed in user interface, then create corresponding field in struct enabled-field in boom-actions module
; 2 - define its hard-coded default value in module boom-parameters. Call it default-k, where k is parameter's name
; 3 - create an entry in config.txt. Call it cfg-k
; 4 - define-config-param accessor with name cfg-k in boom-config. Its default value must be default-k
; 5 - in args module create a command line accessor k. Its default value must be cfg-k
; 6 - change actions% init-field declaration in boom-actions so that its default value is the command line accessor

; the command line accessor will be the parameter initial value when setting up action% control.
; if action% is showing, this initial value can then be overriden by report file and user.
; Otherwise, this initial value is the one in use for the rest of session.

; modules define a debug-module field, which can be toggled, so as to debug particular modules. Its
; default value is controlled by debugger-on, itself defined in boom-parameters.


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


; Mutable global variable that holds report details window current status
; needed for discovery-button%
; altered by report-button
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


; font% constructor is polymorphic. If 2nd argument is a symbol, it will be interpreted as a family. If it is a string, it will be interpreted as a face value
(define header-font%
  (make-object font% 
    (config-head-font-size) 
    (config-head-font-face)))


; main form
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


; holds some picture, which should ideally identify caller app, but can be anything that the caller tastes.
(define icon-canvas
  (let* ([icon (read-bitmap (main-icon))])
    (let-values ([(icon-width icon-height) (values (send icon get-width) (send icon get-height))])
      (new canvas%
           [parent header-panel]
           [min-height icon-height]
           [min-width icon-width]
           [style (list 'transparent)]
           [paint-callback (λ(canvas dc)
                             (send dc draw-bitmap icon 0 0))]))))      
         
    
; User-friendly prompt for informing about caller crash
(define user-friendly-prompt
  (new message% [parent header-panel]
       [label (let ([default-message (~a "Sorry, "
                                         (caller-name)
                                         (rstr 'cpbody replace-cpbody))]) ; This the default algorithm
                
                (with-handlers ([exn:fail? (λ(e)
                                             
                                             (warn-config-exception e)
                                             (cnd-debug
                                              (~a "Exception, while checking uft: " e))
                                             
                                             default-message)])
                     
                  (let ([mask-analysis (eval-user-friendly-text)])

                    (if (uft-evaluation-done? mask-analysis)
                        (begin
                          ; sends warning if user-friendly-mask value is incorrect
                          ; no else. The else case is when user-friendly-mask value is ok
                          (case (uft-evaluation-flag-status mask-analysis)
                            ['set-flag (warn-ftm #t)]
                            ['unset-flag (warn-ftm #f)])

                          (cnd-debug
                           (~a "algorithm: " (uft-evaluation-algorithm mask-analysis)))
                               
                          (case (uft-evaluation-algorithm mask-analysis)
                            ['default default-message]
                            ['plain (user-friendly-text)]
                            ['resource (let* ([rk (rk-from-uft)] ; extracts resource key from value
                                              ; checks whether resource is mask
                                              [rmev (if (non-empty-string? rk)
                                                        (resource-is-mask? rk)
                                                        (void))])
                                         (cond
                                           [(eq? rmev #t) (format (rstr rk "~a")
                                                                  (caller-name))] ; normally the "~a" default string should never be used, since we checked above, that the key pointed to something
                                           [(eq? rmev #f) (rstr rk default-message)]
                                           [else default-message]))]
                            [else (format
                                   (user-friendly-text)
                                   (caller-name))]))
                        (begin
                          (warn-ft-config)
                          default-message)))))]
       [font header-font%]
       [horiz-margin default-spacing]
       [auto-resize #t]))


; all controls, which relate to crash report view
; = permanent controls + optionally visible controls
(define report-panel
  (new vertical-panel%
       [parent boom-window]
       [horiz-margin default-spacing]))


(define (get-hide-caption)
  (rstr 'rblh replace-rblh))


; a button for managing a component's visibility
; It alternatively hides and displays the companion component
; companion must be an area<%>. void is allowed, in which case clicking the button will have no effect.
; Neither will it the area<%> has no parent

; depends on config parameter report-at-start
(define discovery-button%
  (class button%
    (init-field
     [start (verbose-start)])

    ; #t if companion is attached, #f if detached
    (define companion-status start)
    
    ; holds a reference to the object, which must be hide/displayed
    (define companion (void))
    ; holds a reference to the object's parent.
    ; needed for geometry to be recomputed
    (define companion-parent (void))


    (define/public (attach-companion)
      (unless (void? companion)
        (set! companion-status #t)
        (send this set-label (get-hide-caption))

        (unless (void? companion-parent)
          (send companion-parent add-child companion))))

    
    (define/public (detach-companion)
      (unless (void? companion)
        (set! companion-status #f)
        (send this set-label (rstr 'rbls replace-rbls)))

      (unless (void? companion-parent)
        (send companion-parent delete-child companion)))


    (define (inverse-companion-status)
      (if companion-status
          (detach-companion)
          (attach-companion)))
      

    (define (capture-click button event)
      (inverse-companion-status))


    (define/public (get-status)
      companion-status)


    (define/public (set-companion c)
      (let ([clear-companion (λ()
                               (set! companion (void))
                               (set! companion-parent (void))
                               #t)]
            (assign-checked-companion (λ(checked-companion)
                                        (set! companion checked-companion)

                                        (let ([p (send checked-companion get-parent)])
                                          (set! companion-parent p))
                                        #t)))
                                        
        (cond
          [(void? c) (clear-companion)]
          [(is-a? c area<%>) (assign-checked-companion c)]
          [else #f])))
    
    (super-new [callback capture-click]
               [vert-margin default-spacing])))

(define report-button
  (new discovery-button%
       [parent report-panel]
       [label (get-hide-caption)]
       [start #t]))


; crash report view: this panels contains all optionaly visible controls
(define optional-report-items
  (new vertical-panel%
       [parent report-panel]
       [horiz-margin default-spacing]))


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
       [parent optional-report-items]
       [spacing 0]
       [stretchable-height #f]))


; if this is false, there is a coding issue
(define report-containers-attached?
  (send report-button set-companion optional-report-items))


(unless report-containers-attached?
  (log-code-issue "report container not attached"))


; on initializing, report-button consult options
; get-status call let us know about possible disabling of crash report controls in configuration at start
(unless
    (send report-button get-status)
  ; crash report fields have been disabled in configuration
  ; We configure the report-panel accordingly
  (send report-button detach-companion))

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
       [parent optional-report-items]
       [spacing 0]
       [vert-margin 0]))

(define time-field
  (make-time-display meta-panel))


; this fields displays caller app's environment
(define os-field
  (new message%
       [parent meta-panel]
       [label (string-join
               (list (rstr 'os_prompt replace-os-prompt)
                     (os-reading-value
                      (read-report-os (report-buffer-reading crash-data)))))]))


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


; procedure in charge of deleting report file
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