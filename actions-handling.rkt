#lang racket

; pipe which turns report action flags into a control

(require
  "boom-actions.rkt")


(struct reader-config(token validation destination))


(define action-flags
  ; item ordered same as fields in struct boom-actions:enable-field
  (list (reader-config 'tracker (void) 'send)
        (reader-config 'restart (λ(f) (file-exists? f)) 'restart)
        (reader-config 'dau (void) 'cleanup)))


(define (has-action? report action-key more-checks)
  (if (hash-has-key? report action-key)
      (let ([value (hash-ref
                    report
                    action-key
                    '())])
        (if (string? value)
            (if (procedure? more-checks)
                (more-checks value)
                #t)
            #t))
      #f))           


(define (collect-actions report)
  (map  (λ(item)
          (has-action? report
                       (reader-config-token item)
                       (reader-config-validation item)))
        action-flags))


(define (any-action-enabled? flag-list)
  (let ([not-enabled-list (filter (λ(item) (not item)) flag-list)])
    (< (length not-enabled-list)
       (length flag-list))))


(define (make-actions-config-from flags-list)
  (enabled-field (car flags-list)
                 (second flags-list)
                 (third flags-list)))
             

; using report file in order to detect which actions to display
(define (display-actions container report)
  (let ([extraction (collect-actions report)])
    (when (any-action-enabled? extraction)
      (make-actions container
                    (make-actions-config-from extraction)))))



(provide display-actions)
