#lang racket

(require
  "strings-manager.rkt"
  "replacements.rkt")

(define (substitute-report)
  (let ([u (rstr 'unknown replace-unknown)])
    (make-hasheq
     (list
      (cons 'time (current-milliseconds))
      (cons 'description (rstr 'reprep replace-reprep))
      (cons 'software u)
      (cons 'version u)
      (cons 'os  (symbol->string (system-type 'os*)))))))


(provide substitute-report)