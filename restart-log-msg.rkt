#lang racket

; these string parameters are not defined in boom-parameters, because they are not to be changed in config.
; As the restart diaru is designed so as to be processed by automatic tools, they are not to be localized either and
; are not resource strings

; string, which recorded in the file, when the record-maker was provided an empty string instead of an executable path
(define exe-missing "file name argument missing")


(provide
 exe-missing)