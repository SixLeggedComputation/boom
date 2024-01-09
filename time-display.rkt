#lang racket

(require racket/gui
         racket/date
         "strings-manager.rkt"
         "replacements.rkt"
         "time-reader.rkt"
         "boom-parameters.rkt"
         "ghost.rkt")

; controls, which display time of report and an indicator to be used in case of a bad reading


(define time-prompt
  (string-append*
   (list
    (rstr 'time replace-time)
    (colon))))


(define time-prompt-len
  (+ (string-length time-prompt) (ddisp-len default-dates)))

(define flags-spacing 5)


(define/contract (flag-bitmap? tested caller-symbol)
  (-> any/c symbol? boolean?)
  
  (cond
    [(icon-info? tested) #t]
    [(string? tested) #f]
    [else
     (raise-type-error caller-symbol
                       "String or bitmap%"
                       tested)]))
  

; checks type of current-date-system icon field
; only icon-info or string are allowed
; if type requirement are met, this flag is set and its value is used by other methods of this module
; otherwise type-error is fired
(define date-flag-bitmap?
  (flag-bitmap? (ddisp-icon current-date-system)
                'date-flag-bitmap?))


; icon info type checking flag
; checks if data type of good-date-indicator binding matches what is taken by icon->label (string or icon-info)
; failing to set this flag fires type-error
(define green-date-flag?
  (flag-bitmap? good-date-indicator 'green-date-flag?))


; icon info type checking flag
; checks if data type of wrong-date-indicator binding matches what is taken by icon->label (string or icon-info)
; failing to set this flag fires type-error
(define red-date-flag?
  (flag-bitmap? wrong-date-indicator 'red-date-flag?))


; endpoint of icon loading toolchain
; takes type control flag and setls label or lets it unchanged according to flag value
(define/contract (icon->label type-control-flag source)
  (-> boolean?
      (or/c icon-info? string?)
      (or/c (is-a?/c bitmap%) string?))
  
  (if type-control-flag
      (icon-info-bitmap source)
      source))


(define time-display%
  (class horizontal-panel%
    (init-field
     [current-stamp (time-reading 'ok manager-start-time)]
     [time-font normal-control-font])
    
    (super-new [vert-margin 0])

    (define time-label
      (new message%
           [parent this]
           [label (rstr 'time replace-time)]
           [auto-resize #t]
           [font time-font]))

    (define flag-label
      (new message%
           [parent this]
           [label (let ([content (ddisp-icon current-date-system)])
                    (icon->label date-flag-bitmap? content))]
           [horiz-margin flags-spacing]))


    (define (select-correctness correct?)
      (if correct?
          (values green-date-flag? good-date-indicator)
          (values red-date-flag? wrong-date-indicator)))


    (define correctness
      (let ([f (icon->label red-date-flag? wrong-date-indicator)])
        (new message%
             [parent this]
             [label f]
             [horiz-margin flags-spacing])))

    (define/public (update-display new-stamp)
      (if (time-reading? new-stamp)
          ; accepted is set when date struct has been successfully created
          (let-values([(new-date accepted)
                       (with-handlers ([exn:fail? 
                                        (λ(e) 
                                          (values 
                                           (seconds->date manager-start-time) 
                                           #f))])
                         (values 
                          (seconds->date (time-reading-seconds new-stamp)) 
                          #t))])

            ; if not accepted display and field value are unchanged
            (when accepted
              ; displays date as string
              ; date display format is set globally by boom.rkt
              (send time-label set-label (~a time-prompt
                                             (date->string new-date #t)))

              (set! current-stamp new-stamp))
            ; inform client about assignation success 

            ; now, let's display correctness
            (let-values ([(flag indicator)
                          (select-correctness
                           (and accepted
                                (time-reading-status new-stamp)))])
              (send correctness
                    set-label
                    (icon->label flag indicator)))
             
            accepted)
          #f))

    (update-display current-stamp)))


(define (make-time-display
         container
         [start-font normal-control-font])
  ; defines varaiables, that are used for computing text-display% initial width and height
  (let* ([start-font-size (send start-font get-size)]
         [text-area-dims (text-size time-prompt-len 
                                    start-font-size)]
         [widen (λ(n) (+ n (* 2 default-spacing)))]

         [format-flag-width (if date-flag-bitmap?
                                (icon-info-width
                                 (ddisp-icon current-date-system))
                                ; no icon: computes replacement text pixel width
                                (text-size
                                 (string-length
                                  (ddisp-icon current-date-system))
                                 start-font-size))]
                                
         [start-width (+ (widen (car text-area-dims))
                         (* (+ (* 2 flags-spacing) format-flag-width) 2))]
         [start-height (widen (car (cdr text-area-dims)))])
    
    (new time-display%
         [parent container]
         [min-width start-width]
         [min-height start-height])))


(provide
 make-time-display
 time-display%)
