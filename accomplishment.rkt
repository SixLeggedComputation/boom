#lang racket

(require
  "boom-parameters.rkt"
  "misc.rkt")


(define task-outcome
  (list 'not-selected 'failed 'completed))


(define (task-outcome? value)
  (not
   (not-in-list value task-outcome)))


(define (make-task-path icon-name)
  (~a task-icons-dir icon-name))


(define task-icons
  (let ((list-keys (list 'not-selected 'failed 'completed))
        (list-names (map (λ(n) (make-task-path n))
                           (list "task-deselected.png"
                                 "task-failed.png"
                                 "task-achieved.png")))
        (result (make-hasheq)))

    (map (λ(k v)
           (hash-set! result
                      k
                      (load-boom-icon v)))
         list-keys
         list-names)

    result))


(provide
 task-outcome
 task-outcome?
 task-icons)