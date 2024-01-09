#lang racket

(require
  racket/gui
  "boom-parameters.rkt"
  "accomplishment.rkt"
  "strings-manager.rkt"
  "replacements.rkt")


(define debug-module #f)

(define end-window-dims
  (list 350 250))

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
     [label "summary"]
     [parent #f]
     [min-height (car end-window-dims)]
     [min-width (cadr end-window-dims)])

    (define/private (make-task-panel container task-description task-status)
      (if (task-outcome? task-status)
          (let* ([item-panel (new horizontal-panel%
                                  [parent container])]
                 [summary-hint (new message%
                                    [parent item-panel]
                                    [label task-description]
                                    [auto-resize #f]
                                    [min-width 150])]
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


    (define head-label
      (new message%
           [parent this]
           [label (rstr 'endhead replace-endhead)]))
          

    (define info-panel
      (new vertical-panel%
           [parent this]
           [horiz-margin 20]))

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
                       (send this show #f))]))


    (define/public (get-task-status)
      operations-status)


    (define/public (set-status new-status)
      ; both lambda setters must return boolean
      (let* ((set-with-restoration (λ(arg) ; setter to be used when wrong input lets field unchanged
                                     (let ((backup operations-status))
                                       (set! operations-status arg)
                                       (if (valid-status?)
                                           #t
                                           (begin
                                             (set! operations-status backup)
                                             #f)))))
             (set-without-restoration (λ(arg) ; setter to be used when field switches to #f in case of wrong input
                                        (set! operations-status arg)
                                        (if (valid-status?)
                                            #t
                                            (begin
                                              (set! operations-status #f)
                                              #f)))))
        
        ((if restore-when-wrong
             set-with-restoration
             set-without-restoration) new-status)))))


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


(provide show-end)
