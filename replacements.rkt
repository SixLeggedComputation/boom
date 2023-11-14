#lang racket

; string to be displayed, when sme error prevents accessing report content
(define replace-report "No information available")
(define replace-cdnf "Crash report")
(define replace-smip "our software")
(define replace-cpbody " has just crashed.\n\nWe apologize for the inconvenience.\n\nYou may want to see the report or send it to us")
(define replace-capexit "close")
(define replace-capsend "send")
(define replace-restart "restart")
(define replace-actbox "More actions")
(define replace-rblh "hide")
(define replace-rbls "show")
(define replace-acthelp "Selected tasks will be completed, once you have clicked close button")

(provide (all-defined-out))