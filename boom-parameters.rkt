#lang racket

(define default-spacing 20) ; display parameters which sets controls horiz and vert margins
(define default-icon "/home/ml/Documents/boom/boom2.png")
(define default-report-file "crash.json") ; were the report to be displayed should be put, when no file location is provided in command line

(provide
 default-icon
 default-report-file
 default-spacing)