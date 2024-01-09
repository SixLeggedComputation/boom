#lang racket


; loading restart diary


(define (read-log-line log-port content)
  (let ([l (read-line log-port)])
    (if (eof-object? l)
        content
        (read-log-line log-port
                       (append content
                               (list l))))))
          


(define (load-rsl diary)
  (let ([in (open-input-file diary
                             #:mode 'text)])
    
    (read-log-line
     in
     (list))))



(provide load-rsl)
