#lang racket

; a fake report, that is used when the expected one is unreachable or unreadable

(require
  "strings-manager.rkt"
  "replacements.rkt")

(define (substitute-report)
  (make-hasheq
   (list
    (cons 'time (current-milliseconds))
    (cons 'description (rstr 'reprep replace-reprep))
    (cons 'software (rstr 'ucall replace-ucall))
    (cons 'version (rstr 'unknown replace-unknown))
    (cons 'os  (symbol->string (system-type 'os*))))))


(provide substitute-report)