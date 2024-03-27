#lang racket


(define/contract (last-char? src c)
  (-> string? char? boolean?)
  
  (let ([sl (string-length src)])
    (if (> sl 1)
        (eq? (string-ref src
                         (- sl 1))
             c)
        #f)))


(define (test-last-char1)
  (last-char? "a." #\.))


(define (test-last-char2)
  (last-char? "ab" #\c))


(define racket-font-family
  (list 'default 'decorative 'roman 'script 'swiss 'modern 'symbol 'system))


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


; only symbol and non empty string arguments can yield a #t result
(define/contract (racket-font-family? f)
  (-> any/c boolean?)
  
  (let-values ([(test-canceled? tested-value) (cond
                                                [(symbol? f) (values #f f)]
                                                [(non-empty-string? f) (values #f
                                                                               (string->symbol f))]
                                                [else (values #t f)])])
    (if test-canceled?
        #f
      
        (not
         (not-in-list tested-value racket-font-family)))))


(define/contract (me-or test me alternative)
  (-> procedure? any/c any/c any/c)
  
  (if (test me) me alternative))


; turns a list of numbers into a list of exact-integers
(define (round-list l)
  (map (λ(n)
         (exact-round n))
       l))


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
 last-char?
 me-or
 neither
 not-in-list
 racket-font-family
 racket-font-family?
 round-list
 string-upcase-initial)