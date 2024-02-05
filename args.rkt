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
  racket/gui
  "boom-parameters.rkt"
  "logging.rkt")


(define config-file
  (make-parameter #f))
                

(define destroy-after-use
  (make-parameter
   (destroy-after-usage)))


(define my-icon
  (make-parameter
   (main-icon)))


(define local-queue
  (make-parameter #f))


(define skip-end
  (make-parameter #f))


(define report-file-location
  (make-parameter default-report-file))

(define report-file-assignment%
  (class object%
    (super-new)

    (define-values (value)
      (values 'no-assignment))
   
    (define (assign-value new-value)
      (if (or (eq? new-value 'no-assignment)
              (eq? new-value 'rejected)
              (eq? new-value 'valid))
          (begin
            (set! value new-value)
            #t)
          #f))
    (private assign-value)
    
    (define/public (set-value! new-value)
      (unless (assign-value new-value)
        (raise-argument-error
         'new-value
         "one of 'no-assignment 'rejected 'valid"
         new-value)))

    (define/public (anything-to-download?)
      (not (eq? value 'rejected)))


    (define/public (get-value)
      value)))


; status variable, which informs client, in case report file argument could not be assigned or was not included in command line
(define report-status
  (new report-file-assignment%))


(define report-raw-url (void))
    

(define (decode-boolean reading default)
  (cond
    [(string? reading) (if (or (string=? reading "0")
                               (string-ci=? reading "#f")
                               (string-ci=? reading "f"))
                           #f
                           #t)]
    [(number? reading) (if (= reading 0) #f #t)]
    [(boolean? reading) reading]
    [else default]))


; checks report -r argument, assigns report-file-location if possible and returns a flag
(define (assign-report command-line-reading)
  (let ([assignment-status #f])
    (when (path-string? command-line-reading)
      (when (and (path-has-extension? command-line-reading report-extension)
                 (file-exists? command-line-reading))
        (set! assignment-status #t)
        (report-file-location command-line-reading)))

    assignment-status))


; converts assign-report flag to status value
(define (set-report-status-from-entry command-line-reading)
  (set! report-raw-url command-line-reading)
  
  (send report-status
        set-value!
        (if (assign-report command-line-reading)
            'valid
            'rejected)))


(define/contract (icon-ok? location)
  (-> string? boolean?)
  
  (let ([test-bitmap (with-handlers ([exn:fail? (λ(e) #f)])
                       (make-object bitmap% location))])
    (if (boolean? test-bitmap)
        #f
        (send test-bitmap ok?))))


(define (assign-my-icon proposal)
  (if (icon-ok? proposal)
      (my-icon proposal)
      (log-invalid-icon proposal)))
         
      

(define command-line-parser
  (command-line
   #:usage-help
   "Boom utility software is invoked by crashed applications, so as to let you know about their failure and eventually report it to developpers."
   
   #:once-each
   [("-c" "--cfg") new-config
                   "path to config file"
                   (config-file new-config)]
   [("-d" "--dau") new-dau
                   "destroy after usage. When this parameter is set to true (default) and permission is granted to the manager, report file are deleted, when boom closes."
                   (destroy-after-use
                    (decode-boolean new-dau
                                    (destroy-after-usage)))]
   [("-i" "--icon") new-icon
                    "provides an icon in order to enrich display"
                    (assign-my-icon new-icon)]
   [("-s" "--skipend") new-ending
                       "skips or enables end summary display"
                       (skip-end
                        (decode-boolean new-ending
                                        (show-summary)))]
   [("-l" "--lq") default-queue
                  "local queuing system for storing reports, if any.
When used this parameter serves as default. When not used, it is considered, there is no local queue by default.
This behavior can be altered by including lq parameter in report files"
                  (local-queue default-queue)]
   [("-r" "--report") custom-report
                      "provides report location. This parameter should be a full or relative path, including file name"
                      (set-report-status-from-entry custom-report)]

   #:args () ; expect one command-line argument: a report location

   (void)))

(provide
 destroy-after-use
 my-icon
 report-file-location
 report-raw-url
 report-status
 skip-end)
