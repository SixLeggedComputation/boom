#lang racket

; handles command-line arguments
; all command line arguments are optional
; except for -c, these parameters can be populated from config file. Precedence is:
; command line > config file > default
; command line:
; -c --cfg path to config file
; -d --dau destroy after usage. When this parameter is set to true (default) and permission is granted to the manager, report file are deleted, when it closes.
; -r --report report file location (value includes full or relative path + filename)
; -i --icon icon location
; -l --lq local queuing system for storing reports, if any. When used this parameter serves as default. When not used, it is considered, there is no local queue by default. This behavior can be altered by including lq parameter in report files

(require
  racket/cmdline
  "boom-parameters.rkt"
  "boom-config.rkt")


(define config-file
  (make-parameter #f))
                

(define destroy-after-use
  (make-parameter
   (cfg-dau)))


(define my-icon
  (make-parameter default-icon))


(define local-queue
  (make-parameter #f))


(define skip-end
  (make-parameter #f))


(define report-file-location
  (make-parameter default-report-file))


(define command-line-parser
  (command-line
   #:usage-help
   "boom utility software is invoked by crashed applications, so as to let you know about their failure and eventually report it to developpers."

   #:once-each
   [("-c" "--cfg") new-config
                   "path to config file"
                   (config-file new-config)]
   [("-d" "--dau") new-dau
                   "destroy after usage. When this parameter is set to true (default) and permission is granted to the manager, report file are deleted, when it closes."
                   (destroy-after-use new-dau)]
   [("-i" "--icon") new-icon
                    "provides an icon in order to enrich display"
                    (my-icon new-icon)]
   [("-s" "--skipend") new-ending
                       "skips or enables end summary display"
                       (skip-end new-ending)]
   [("-l" "--lq") default-queue
                  "local queuing system for storing reports, if any.
When used this parameter serves as default. When not used, it is considered, there is no local queue by default.
This behavior can be altered by including lq parameter in report files"
                  (local-queue default-queue)]
   [("-r" "--report") custom-report
                      "provides report location. This parameter should be a full or relative path, including file name"
                      (report-file-location custom-report)]

   #:args ()

   (void)))


(provide
 destroy-after-use
 my-icon
 report-file-location
 skip-end)
