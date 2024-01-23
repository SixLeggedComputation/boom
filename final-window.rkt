#lang racket

(require
  racket/gui
  scribble/text/wrap
  "boom-parameters.rkt"
  "accomplishment.rkt"
  "strings-manager.rkt"
  "replacements.rkt"
  "ghost.rkt"
  "controls.rkt")


; Flag, which can be set when debugging module, so as to get some displays.
(define debug-module debugger-on)

; state variable: informs main module, that user does not want this window any more
; initialized with #f. If show-end is called, it means that summary window option was selected
; in configuration
(define discarded #f)

; window aspect width and height aspect ratio on build screen
; used for fitting window into production screen
(define end-window-ratios
  (list
   (/ 35 192)
   (/ 25 108)))


; fits window into screen
(define end-window-dims
  (let-values (((cur-width cur-height) (get-display-size)))
    (list
     (*
      (car end-window-ratios)
      cur-width)
     (* (cadr end-window-ratios)
        cur-height))))


(define operations-label-width-ratio
  (/ 3 7))


(define operations-label-margin-ratio
  (/ 2 35))


(define operations-label-width
  (* (car end-window-dims) operations-label-width-ratio))


(define operations-label-margin
  (* operations-label-margin-ratio
     (car end-window-dims)))


(define (status-hash? tested-hash)
  (let ((result #f)
        (hash-items-tests (λ(k v)
                            (if (or (eq? 'restart k)
                                    (eq? 'cleanup k)
                                    (eq? 'send k))
                                (task-outcome? v)
                                #f)))
        (passed? (λ(items-result-list)
                   (andmap (λ(t) t)
                           items-result-list))))
    
    (when (hash? tested-hash)
      (let ((tests (hash-map tested-hash hash-items-tests)))
        (set! result
              (passed? tests))))
    result))


(define task-hint
  (hasheq 'send (rstr 'send-data replace-send-data #t)
          'cleanup (rstr 'cleanup replace-cleanup #t)
          'restart (rstr 'caprestart replace-restart #t)))


(define last-dialog%
  (class dialog%
    (init-field [operations-status #f]
                [restore-when-wrong #t]) ; behavior when set-status fails. #t : operations-status is restored to its previous value. #f : operations-status is set to #f

    (super-new
     [label (rstr 'endlabel replace-endlabel)]
     [parent #f]
     [min-height (car end-window-dims)]
     [min-width (cadr end-window-dims)])

    (define/private (make-task-panel container task-description task-status)
      (if (task-outcome? task-status)
          (let* ([item-panel (new horizontal-panel%
                                  [parent container])]
                 [summary-lines (wrap-line task-description
                                           operations-label-width)]
                 [summary-height (fit-height (length summary-lines)
                                             ghost-sys-font-size)]
                 [summary-with-jumps (string-join summary-lines "\n")]
                 [summary-hint (new message%
                                    [parent item-panel]
                                    [label summary-with-jumps]
                                    [auto-resize #f]
                                    [min-width operations-label-width])]
                 [summary-status (new message%
                                      [parent item-panel]
                                      [label (let ([label-content (hash-ref task-icons
                                                                            task-status
                                                                            bitmap-default-alt)])

                                               (when debug-module
                                                 (display
                                                  (~a "nmake-task-panel. Recherche de "
                                                      task-status
                                                      " dans la librairie d'icônes "
                                                      task-icons
                                                      "\nRésultat= "
                                                      label-content)))
                                               
                                               (if (icon-info? label-content)
                                                   (let ((image-url (icon-info-bitmap label-content)))
                                                     (when debug-module
                                                       (display
                                                        (~a "Chemin fichier = "
                                                            image-url)))
                                                     image-url)
                                                   label-content))])])
            item-panel)

          (raise-argument-error "task-status"
                                "task-outcome"
                                2
                                task-status)))


    (define/private (valid-status?)
      ; checks if operation-status is initialized with its default value
      (let ((default-status? (λ()
                               (if (boolean? operations-status)
                                   (not operations-status) ; only #f is allowed. So #t yields false
                                   #f))))
        (if (default-status?)
            #t
            (status-hash? operations-status))))


    (define/private (get-status key)
      (if (valid-status?)
          (if (hash? operations-status)
              (hash-ref operations-status
                        key
                        'not-selected)
              'not-selected)
          'not-selected))


    (define head-text
      (new text-canvas%
           [parent this]
           [unformatted (rstr 'endhead replace-endhead)]
           [font-size 10]
           [color-name "black"]
           [background-name "Ghost White"]
           [min-width 300]
           [stretchable-width #f]
           [stretchable-height #t]))
          

    (define info-panel
      (new vertical-panel%
           [parent this]
           [horiz-margin operations-label-margin]))

    (hash-for-each task-hint
                   (λ(k v)                     
                     (make-task-panel info-panel
                                      v
                                      (get-status k))))

    (define discard-panel
      (new horizontal-panel%
           [parent this]))

    (define discard-check
      (new check-box%
           [parent discard-panel]
           [label (rstr 'skipwind replace-skipwind)]))

    (define cmd-panel
      (new horizontal-panel%
           [parent this]
           [alignment '(center center)]))


    (define end-button
      (new button%
           [parent cmd-panel]
           [label (rstr 'capexit replace-capexit)]
           [callback (λ(b e)
                       (set! discarded
                             (stop-showing))
                       (send this show #f))]))


    (define/public (get-task-status)
      operations-status)

    ; returns true if user hits the "don't show anymore" checkbox
    (define (stop-showing)
      (send discard-check get-value))))


(define test-window
  (new last-dialog%))


(define test-state
  (let ((result (make-hasheq)))
    (hash-set! result  'send 'not-selected)
    (hash-set! result 'cleanup 'completed)
    (hash-set! result 'restart 'not-selected)
    result))  


(define (show-end [status #f])
  (let ((end-window (new last-dialog%
                         [operations-status status])))

    (when debug-module
      (display
       (send test-window get-task-status)))
    
    (send end-window show #t)))


(define (test-show)
  (set! debug-module #t)
  (show-end test-state))

(provide
 discarded
 show-end)
