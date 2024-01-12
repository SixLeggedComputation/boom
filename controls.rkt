#lang racket

; basic controls which are derived from racket/gui classes

(require
  racket/gui
  "text-processing.rkt"
  "boom-parameters.rkt"
  "strings-manager.rkt"
  "replacements.rkt"
  )


(define (make-close-button caption container form [action-handler (void)])
  (new button%
       [label caption]
       [parent container]
       [callback (λ(button event)
                   
                   (when (procedure? action-handler)
                     (action-handler))
                   
                   (send form show #f))]))


(define text-canvas%
  (class canvas%
    (init-field [unformatted #f])


    (define (paint-text canvas dc)
      (send dc clear)
      (send dc set-text-foreground "red")

      (let* ([formatted-text (clip-text canvas unformatted '(5 5))]
             [fs (ceiling
                  (send
                   (send dc get-font)
                   get-size
                   #t))] ; gets current font height as an integer
             [pix-spacing (ceiling
                           (* default-line-spacing fs))]
             [ylocations (λ(nb-lines)
                           (map
                            (λ(n) (+ (* n (+ pix-spacing fs)) 5))
                            (range nb-lines)))] ; computes text lines y coordinates. nb-lines is number of lines obtained from clipping procedure
             [display-line (λ(x y s)
                             (send dc draw-text s x y 'grapheme))] ; primitive function for displaying a string to some canvas position
             [display-one (λ(s)
                            (display-line 5 5 s))])                ; shortcut to be used there is just one string to be displayed: prints string to top location 

        (if (list? (clipped-text formatted-text)) ; if clipping sucessed, text is returned as a list of strings, even when there is only 1 line
            (let ([par-lines (length (clipped-text formatted-text))])
              (cond
                [(= 0 par-lines)
                 (display-one (rstr 'msgparser replace-msg-parser))]
                [(= 1 par-lines)
                 (display-one (car (clipped-text formatted-text)))]

                [else
                 (map (λ(s y)
                        (display-line (clipped-left formatted-text) y s)
                        #t)
                      (clipped-text formatted-text)
                      (ylocations par-lines))]))
        
            (when (string? unformatted)
              (display-one unformatted)))))


    (super-new
     [callback paint-text])))

(provide
 make-close-button
 text-canvas%)
