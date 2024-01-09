#lang racket

; this module runs an invisible form, that is used for computing strings screen representation size before they are displayed, so as to manage proper space for them

(require racket/gui)


; builds a test string as a mixture of wide and descending characters, in order to estimate future screen size a string.
; If char-num is 1 then test string involves only 1 wide character
(define/contract (test-string char-num)
  (-> exact-positive-integer? string?)

  (if (= 1 char-num)
      "A"
      (let-values ([(s1 carry) (quotient/remainder char-num 2)])
        (let* ([s2 (+ s1 carry)]
               [low-str (make-string s1 #\g)]  ; substring made out of descending characters
               [high-str (make-string s2 #\A)]) ; substring made out of wide characters
          (string-append* (list low-str high-str))))))

; we use a test canvas, which holds a drawing context. This is its parent window.
(define ghost-frame 
  (new frame% [label "test"]))


(define ghost-canvas 
  (new canvas% [parent ghost-frame]))


(define ghost-dc
  (send ghost-canvas get-dc))


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


(provide text-size)