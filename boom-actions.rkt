#lang racket

(require racket/gui
         "boom-parameters.rkt"
         "strings-manager.rkt"
         "replacements.rkt"
         "args.rkt")


(define key-send 'send)
(define key-restart 'restart)
(define key-cleanup 'cleanup)


(define-syntax-rule (make-struct-dyn name fields)
  #'(struct name fields))


(define (struct/dyn name fields)
  (eval (make-struct-dyn name fields)))


; in case field ordering is changed, see module actions-handling
; field names used as keys in actions-catalog
(struct enabled-field (send restart cleanup))
;(struct/dyn enabled-field (key-send key-restart key-cleanup))


; struct, which gathers information for building actions checkboxes
; stores caption and initial value, which the user can change in the interface
(struct acntdata(key caption value))


(define actions-catalog
  (list
   (acntdata 'send
             (rstr 'capsend replace-capsend)
             #f)
   (acntdata 'restart
             (rstr 'caprestart replace-restart)
             #t)
   (acntdata 'cleanup
             (rstr 'cleanup replace-cleanup)
             (destroy-after-use))))


(define (action-data k)
  (for/first
      ([d actions-catalog]
       #:when (eq?
               (acntdata-key d)
               k))
    d))


; structure pooling together alarm flags
; invactkey = invalid action key = a programming error was detected
; funarg = some function received unexpected argument value
(struct ereg([invactkey #:mutable]
             [funarg #:mutable]))
             

(define actions%
  (class group-box-panel%
    ; enabled-fields sets or unsets check boxes at start
    ; the executable to be restarted can be provided via boom command-line
    ; init value for restart must be #f. If the executable to be restarted is not in the crash report, we can't guess what's to be restarted
    (init-field [action-parameters (enabled-field #t #f (destroy-after-use))])

    (field [error-register (ereg #f #f)])
               
    (super-new [label (rstr 'actbox replace-actbox)]
               [alignment '(left center)]
               [vert-margin default-spacing]
               [horiz-margin default-spacing])

    (define boxes-layout
      (new horizontal-panel%
           [parent this]
           [alignment '(center center)]))

    (define/private (make-action-checkbox activated search-key)
      (when activated
        (let([data (action-data search-key)])
          (if (acntdata? data)
              (new check-box%
                   [parent boxes-layout]
                   [label (acntdata-caption data)]
                   [value (acntdata-value data)]
                   [horiz-margin default-spacing])
              
              (begin
                (set-ereg-invactkey! error-register #t)
                (void))))))

    (define/private (make-check-send)
      (make-action-checkbox (enabled-field-send action-parameters)
                            'send))

    (define check-send (make-check-send))

    (define/private (make-check-restart)
      (make-action-checkbox (enabled-field-restart action-parameters)
                            'restart))

    (define check-restart (make-check-restart))

    (define/private (make-cleanup)
      (make-action-checkbox (enabled-field-cleanup action-parameters)
                            'cleanup))

    (define check-cleanup (make-cleanup))


    (define action-help
      (new message%
           [parent this]
           [label (rstr 'acthelp replace-acthelp)]
           [horiz-margin default-spacing]))


    ; which-one: check box object or void if the action is not enabled by crash report
    ; not including an action in crash reprt is same as disarding it, unless it is overriden by command line
    (define/private (read-action-check which-one [cmd-line-value #f])
      (cond
        [(void? which-one) cmd-line-value]
        [(is-a? which-one check-box%) (send which-one get-value)]
        [else (raise-argument-error "which-one"
                                    "void or check-box"
                                    1
                                    which-one)]))


    (define/public (clear-file?)
      (read-action-check check-cleanup
                         (enabled-field-cleanup action-parameters)))


    (define/public (restart?)
      (read-action-check check-restart))))


(define (accepted-container? tested-container)
  (is-a? tested-container area-container<%>))


(define (action-control? tested-variable)
  (is-a? tested-variable actions%))


(define/contract (make-actions container selected-fields)
  (-> accepted-container? enabled-field? action-control?)
  
  (new actions%
       [parent container]
       [action-parameters selected-fields]))


(provide
 actions%
 make-actions
 (struct-out enabled-field)
 (struct-out ereg))