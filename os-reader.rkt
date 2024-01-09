#lang racket

(require "misc.rkt")

; reads os field value from report
; it can be left blank. In this case calling app os is assumed to the same as crash manager environment

(struct os-reading(status value)
  #:guard (λ(init-status init-value name)
            (let* ([find-type (λ(v)
                                (cond
                                  [(symbol? v) "symbol"]
                                  [(string? v) "string"]
                                  [(number? v) "number"]
                                  [(boolean? v) "boolean"]
                                  [(struct? v) "struct"]
                                  [(procedure? v) "procedure"]
                                  [else "unknown"]))]
                   [guessed-value-type (find-type init-value)]
                   [guessed-flag-type (find-type init-status)]
                   [fire-exception (λ(where expected-type type-given value-given)
                                     (raise-argument-error "constructor"
                                                           expected-type
                                                           where
                                                           (format "~a : ~a"
                                                                   type-given
                                                                   value-given)))])

              (when (not (boolean? init-status))
                (fire-exception 1
                                "boolean?"
                                guessed-flag-type
                                init-status))

              (when (not (string? init-value))
                (fire-exception 2
                                "string?"
                                guessed-value-type
                                init-value))

              (values init-status init-value))))


(define (default-os-info)
  (~a (system-type 'os*)
      " "
      (system-type 'arch)))


(define (read-report-os report-hash)
  (let* ([raw-value (if (hash-has-key? report-hash 'os)
                        (hash-ref report-hash
                                  'os
                                  (void))
                        (void))]
         [use-default? (cond
                         [(void? raw-value) #t]
                         [(string? raw-value) (empty-string? raw-value)]
                         [(symbol? raw-value) #f]
                         [else #t])])
    (if use-default?
        (os-reading #f (default-os-info))
        (os-reading #t (if (symbol? raw-value)
                           (symbol->string raw-value)
                           raw-value)))))


(provide
 read-report-os

 (struct-out os-reading))