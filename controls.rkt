#lang racket

; basic controls which are derived from racket/gui classes

(require
  racket/gui
  "text-processing.rkt"
  "boom-parameters.rkt"
  "strings-manager.rkt"
  "replacements.rkt"
  )

(define debug-module debugger-on)


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
    (init-field [unformatted #f]
                [color-name "red"]
                [font-size #f]
                [background-name #f])


    (define/private (my-dc)
      (send this get-dc))


    (define/private (paint-background)
      (when (string? background-name)
        (let* ([dc-instance (my-dc)]
               [old-brush (send dc-instance get-brush)]
               [new-pen (new pen%
                             [style 'transparent])]
               [new-brush (new brush%
                               [color background-name])])
          (let-values ([(clw clh) (send this get-client-size)])
            (when debug-module
              (display (~a
                        "painting background width "
                        clw
                        ", height "
                        clh
                        ", with colour "
                        background-name
                        "\n")))
            (send dc-instance set-brush new-brush)
            (send dc-instance set-pen new-pen)
            (send dc-instance draw-rectangle 0 0 clw clh)
            (send dc-instance set-brush old-brush)))))
                               


    (define (paint-text canvas dc)
      (send dc clear)
      (paint-background)
      (send dc set-text-foreground color-name)

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
     [paint-callback paint-text])

    (when (number? font-size)
      (let* ([current-dc (send this get-dc)]
             [current-font (send
                            current-dc
                            get-font)]
             [new-font (make-object font%
                         font-size
                         (send current-font get-family)
                         (send current-font get-style)
                         (send current-font get-weight)
                         (send current-font get-underlined)
                         (send current-font get-smoothing))])
        (send current-dc set-font new-font)))

    (paint-background)))
        

(provide
 make-close-button
 text-canvas%)
