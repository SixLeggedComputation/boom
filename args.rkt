#lang racket

; handles command-line arguments
; command line:
; -r --report report file location
; -i --icon icon location

(require
  racket/cmdline
  "boom-parameters.rkt")

(define my-icon
  (make-parameter default-icon))

(define report-file-location
  (make-parameter default-report-file))


(define command-line-parser
  (command-line
   #:usage-help
   "boom is a crash reporter"

   #:once-each
   [("-i" "--icon") new-icon
                    "provides an icon in order to enrich display"
                    (my-icon new-icon)]
   [("-r" "--report") custom-report
                      "provides report location"
                      (report-file-location custom-report)]

   #:args ()

   (void)))


(provide
 my-icon
 report-file-location)
