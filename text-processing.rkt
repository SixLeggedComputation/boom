#lang racket

(require
  scribble/text/wrap
  racket/gui
  "boom-parameters.rkt")

; fits texts into containers


(define debug-module debugger-on)


(define/contract (wrapped s nchars [left-padding 0])
  (->* (non-empty-string? exact-positive-integer?)
       (positive-integer?)
       string?)

  (let ([apply-padding (not
                        (or (eq? 0 left-padding)
                            (>= left-padding nchars)))]
        [generate-padding-chars (λ(n)
                                  (make-string
                                   n
                                   (integer->char #x20)))])
    (if apply-padding
        (let* ([cut-pos (- nchars left-padding)]
               [pad (string-join
                     (list "\n"
                           (generate-padding-chars left-padding))
                     "")]
               [result-string (string-join
                               (wrap-line s nchars)
                               pad)])
          (string-join
           (list (generate-padding-chars left-padding)
                 result-string)
           ""))

        (string-join
         (wrap-line s nchars)
         "\n"))))


(define (test-wrapping)
  (wrapped "a a a a a a a a a a a a a a a a a" 10 2))


(define (text-container? tested)
  (is-a? tested canvas%))


(struct fitting(cut left))


; checks margins argument format and fixes it, if needed
; returns a list containing left and right margins
(define (cleaned-margin-list margins)
  (case (length margins)
    [(0) (list 0 0)] ; original argument was empty
    [(1) (list (car margins) 0)] ; one margin value is missing, it is inferred that it is the right margin
    [(2) margins] ; 2 values provided: OK. Nothing is done.
    ; too many values: retains first and second.
    [else (list
           (car margins)
           (cadr margins))]))


; computes space available for text and filters out irrealistic margin values
(define (get-usable-space container-width margins)
  (let* ([usable-list (cleaned-margin-list margins)]
         [total-margin (foldl + 0 usable-list)])

    (if (> container-width 0)
        (if (> container-width total-margin)
            (values
             (- container-width total-margin)
             (car usable-list))
            
            (if (> container-width (car usable-list))
                (values
                 (- container-width (car usable-list))
                 (car usable-list))
                
                (if (> container-width (cadr usable-list))
                    (values
                     (- container-width (cadr usable-list))
                     0)
                    (values container-width 0))))
        (values 0 0))))



(define (margins/c?)
  (listof exact-nonnegative-integer?))


(define/contract (fit-to container margins)
  (-> text-container? (margins/c?) fitting?)
  
  (let-values ([(cntw cnth) (send container get-client-size)])
    (let-values ([(cut-len left-margin) (get-usable-space cntw margins)])
      (let* ([current-dc (send container get-dc)]
             [current-font (send current-dc get-font)]
             [current-size (send current-font get-size #f)])
        
        (when debug-module
          (display (~a "client width= " cntw "\nfont size=" current-size "\ncut len= " cut-len "\n")))
        
        (fitting
         (exact-floor (/ cut-len current-size))    ; if arguments are incorrect cut-len will be 0 and the result will be 0, as a way to alert about error condition
         left-margin)))))

; text field holds either the original string (in case of failure) or a list of clipped strings
; left holds the computed value for left margin, which is either 0 or the original value
; alert is a boolean flag, which informs client about some computations having gone wrong
(struct clipped(text left alert))

(define/contract (clip-text container s margins)
  (-> text-container? string? (margins/c?) clipped?)

  (if (non-empty-string? s)
      (let ([clip-info (fit-to container margins)])
        (if (> (fitting-cut clip-info) 0)                       ; non 0 cut len value says computation is alright. Then proceed to clipping
            (begin
              (when debug-module
                (display
                 (~a "longueur de ligne = " (fitting-cut clip-info) "\n")))
              
              (clipped
               (wrap-line s (fitting-cut clip-info))
               (fitting-left clip-info)
               #f))

            (clipped                                            ; 0 cut len received: some computation went wrong
             s
             0
             #t)))
      ; handles empty input string
      (clipped
       (list s)
       (car margins)
       #t)))


(provide
 clip-text
 wrapped
 (struct-out clipped))