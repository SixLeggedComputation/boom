#lang racket

; internationalization

(require "replacements.rkt")

(define alert-locale-error #f)
(define locale-port #f)

(define locale-directory
  "/home/ml/Documents/boom/resources/locales/")

(define dir-separator
  (if (equal? 'unix (system-path-convention-type))
      "/"
      "\\"))

(define default-resource
  "/home/ml/Documents/boom/resources/locales/en/US.rktd")

(define local-sys
  (system-language+country))


(define sys-lang
  (string-split
   (list-ref (string-split local-sys ".") 0)
   "_"))


(define/contract (open-locale-port)
  (-> (or/c port? boolean?))
    
  (let* ([lang-decoded? (λ(computed)
                          (if (list? computed)
                              (if (= 2 (length computed))
                                  (andmap string? computed)
                                  #f)
                              #f))]
         [searched-file (if (lang-decoded? sys-lang)
                            (~a locale-directory
                                (first sys-lang)
                                dir-separator
                                (last sys-lang)
                                ".rktd")
                            default-resource)]
         [validated-file (if (file-exists? searched-file)
                             searched-file
                             default-resource)])
    (with-handlers ([exn:fail? (λ(ex) #f)])
      (open-input-file validated-file))))


(define (parse-token-list)
  (set! locale-port (open-locale-port))
  (set! alert-locale-error (boolean? locale-port))

  (if alert-locale-error
      '()
      (let ([file-data (read locale-port)])
        (begin0
          (for/list ([token file-data])
            (make-hasheq token))
          (close-input-port locale-port)))))


(define token-list (parse-token-list))


(define search-token/c
  (or/c symbol? string?))

(define/contract (find-resource token-key)
  (-> search-token/c
      (or/c string? boolean?))
  
  (let ([searched (if (symbol? token-key)
                      (symbol->string token-key)
                      token-key)])
    (for/first ([h token-list]
                #:when (string-ci=? (hash-ref h 'key) searched))
      (hash-ref h 'val))))


(define/contract (rstr token-key replacement)
  (-> search-token/c
      string?
      string?)
  
  (let ([search-result (find-resource token-key)])
    (if (boolean? search-result)
        replacement
        search-result)))


(provide
 alert-locale-error
 rstr)
