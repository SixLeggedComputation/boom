#lang racket

; this module runs an invisible form, that is used for computing strings screen representation size before they are displayed, so as to manage proper spacing for them

(require racket/gui)


(define the-large-char #\W)
(define the-deep-char #\g)

(define unit-string
  (make-string 1 the-large-char))


; builds a test string as a mixture of wide and descending characters, in order to estimate future screen size a string.
; If char-num is 1 then test string involves only 1 wide character
(define/contract (test-string char-num)
  (-> exact-positive-integer? string?)

  (if (= 1 char-num)
      (make-string 1 the-large-char)
      (let-values ([(s1 carry) (quotient/remainder char-num 2)])
        (let* ([s2 (+ s1 carry)]
               [low-str (make-string s1 the-deep-char)]  ; substring made out of descending characters
               [high-str (make-string s2 the-large-char)]) ; substring made out of wide characters
          (string-append* (list low-str high-str))))))

; we use a test canvas, which holds a drawing context. This is its parent window.
(define ghost-frame 
  (new frame% [label "test"]))


(define ghost-canvas 
  (new canvas% [parent ghost-frame]))


(define ghost-dc
  (send ghost-canvas get-dc))


(define ghost-sys-font
  (send ghost-dc get-font))


(define ghost-sys-font-size
  (send ghost-sys-font get-size))


(define (make-ghost-font desired-font-size)
  (make-object font%
    desired-font-size
    'system
    'normal
    'normal
    #f
    'default
    #t))


(define (text-dim test test-font)
  (send ghost-dc get-text-extent test test-font))
  

; computes the number of characters, which can be fitted into some width
; available-width in pixels
;desired-font-size in pixels
(define/contract (how-many-x? available-width desired-font-size)
  (-> exact-positive-integer?
      (real-in 0.0 1024.0)
      exact-integer?)
  
  (if (or (= 0 available-width)
          (= 0 desired-font-size))
      0
      (let-values (((cw cy dist-base extra-y) (text-dim unit-string
                                                        (make-ghost-font desired-font-size))))
        (exact-floor
         (/ available-width cw)))))


(define (test-how-many-x)
  (how-many-x? 120 12))


; how much a control must be widened, in order to fit some text
(define/contract (text-size char-num desired-font-size)
  (-> exact-positive-integer?
      (real-in 0.0 1024.0)
      (listof exact-positive-integer?))
  
  (let* ([s (test-string char-num)]
         [ghost-font (make-object
                         font%
                       desired-font-size
                       'system)])
    (let-values ([(w h dist-base design-increment)
                  (send ghost-dc get-text-extent s ghost-font)])
      (list
       (inexact->exact
        (ceiling w))
       (inexact->exact
        (ceiling h))))))


(define (fit-height lines desired-font-size)
  (let ((s (test-string 2))
        (ghost-font (make-ghost-font desired-font-size)))
    (let-values (((w h dist-base design-increment) (send ghost-dc get-text-extent s ghost-font)))
      (exact-ceiling
       (* h lines)))))


(provide
 fit-height
 ghost-sys-font-size
 how-many-x?
 text-size)