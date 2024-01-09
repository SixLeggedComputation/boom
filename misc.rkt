#lang racket


(define/contract (neither a b)
  (-> boolean? boolean? boolean?)

  (not
   (or a b)))


(define/contract (not-in-list v l)
  (-> any/c
      list?
      boolean?)

  (boolean?
   (member v l)))


(define/contract (me-or test me alternative)
  (-> procedure? any/c any/c any/c)
  
  (if (test me) me alternative))


(define (empty-string? s)
  (not
   (non-empty-string? s)))


(define (string-upcase-initial s)
  (if (non-empty-string? s)
      (let* ((words (string-split s))
             (first-word (string-titlecase
                          (car words))))
        (string-join
         (append (list first-word)
                 (cdr words))))
      ""))



(define (test-initial1)
  (string-upcase-initial "le renard saute par dessus la haie"))


(define (test-initial2)
  (string-upcase-initial "a"))
             


(provide
 empty-string?
 me-or
 neither
 not-in-list
 string-upcase-initial)