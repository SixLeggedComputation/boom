#lang racket

; internationalization

(require
  "boom-parameters.rkt"
  "misc.rkt")

(define alert-locale-error #f)
(define locale-port #f)

(define dir-separator
  (if (equal? 'unix (system-path-convention-type))
      "/"
      "\\"))

(define locale-directory
  (~a "resources"
      dir-separator
      "locales"
      dir-separator))

(define default-resource
  (~a locale-directory
      "en"
      dir-separator
      "US.rktd"))

(define local-sys
  (system-language+country))


; returns a list (language country)
(define sys-lang
  (if (eq?
       (system-type)
       'windows)
      ; windows system-type turns out to be "French_France"
      ; this turns is into '(fr FR)
      (let* ([unix-equiv (λ(value)
                           (substring value 0 2))]
             [win-locale-parts (string-split local-sys "_")]
             [lng (string-downcase
                   (unix-equiv
                    (car win-locale-parts)))]
             [country (string-upcase
                       (unix-equiv
                        (cadr win-locale-parts)))])
        (list lng country))
        
      (string-split
       (car
        (string-split local-sys "."))
       "_")))


(define current-date-system
  (if (string-ci=? (last sys-lang) "US")
      us-dates
      default-dates))


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


(define/contract (rstr token-key replacement [upcase-init #f])
  (->* (search-token/c
        string?)
       (boolean?)
       string?)
  
  (let* ([search-result (find-resource token-key)]
         [returned (if (boolean? search-result)
                       replacement
                       search-result)])

    (if upcase-init
        (string-upcase-initial returned)
        returned)))


(define (colon)
  (if (string-ci=?
       (car
        (string-split
         (system-language+country)
         "_"))
       "fr")
      " : "
      ": "))



; checks if resource is a mask
; token-key is same key as in rstr
; returns void when token-key points to no resource, boolean otherwise
(define/contract (resource-is-mask? token-key)
  (-> search-token/c
      (or/c boolean? void?))
  
  (let* ([not-found ""]
         [found (rstr token-key not-found)]) ; no error handler here. The only possible error would be fired by search-token/c. But token-mask already complies to this contract.

    (if (non-empty-string? found)
        (mask? found)
        (void))))


(provide
 alert-locale-error
 colon
 current-date-system
 resource-is-mask?
 rstr)
