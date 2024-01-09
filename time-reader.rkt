#lang racket

; records boom launching time for further validation of report data
(define manager-start-time
  (current-seconds))


(define (reading-status? token)
  (list?
   (member
    token
    (list 'ok 'invalid 'missing 'outside))))


(define (invalid-time? reading strict)
  (or (< reading 0) 
      ((if strict
           >
           >=) reading manager-start-time)))




(struct time-reading(status seconds)
  #:guard (λ(token reading name)
            (when (or (invalid-time? reading #t)
                        (not (reading-status? token)))
              (error
               (format "invalid initialisation values (~a ~a) provided to ~a"
                       token
                       reading
                       name)))

            (values token reading)))


(define (read-report-time report-hash)
  (if (hash-has-key? report-hash 'time)
      (let* ([file-value (hash-ref report-hash
                                   'time
                                   manager-start-time)]
             [translation ((λ(t)
                             (cond
                               [(number? t) t]
                               [(string? t) (string->number t)]
                               [else #f])) file-value)])
             
        (cond
          [(boolean? translation)
           (time-reading 'invalid
                         manager-start-time)]
          [(invalid-time? translation #f)
           (time-reading 'outside
                         manager-start-time)]
          [else        
           (time-reading 'ok
                         (hash-ref
                          report-hash
                          'time
                          manager-start-time))]))

      (time-reading 'missing
                    manager-start-time)))


(provide
 manager-start-time
 read-report-time
 (struct-out time-reading))