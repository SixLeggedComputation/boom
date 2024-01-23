#lang racket

(require
  racket/date
  racket/gui
  config
  "misc.rkt")

; provides default values to all app-specific parameters, which can be specified in config file

; turns on/off dubegging in this module and the others
(define debugger-on #f)

; original screen size. Used for computing frames aspect ratio
(define builder-aspect
  (list 1920 1080))


(define restart-log-file "restart.txt")

; time formatting for restart diary
; this is not shown by ui and therefore needn't be localized
(define restart-log-date 'iso-8601)


(define/contract (edit-config param-name new-value)
  (-> (or/c symbol? string?)
      any/c
      boolean?)
  
  (let* ([config-data (make-hasheq)]
         [search-token (me-or
                        symbol?
                        param-name
                        (string->symbol param-name))]
         [changed #f]
         ; this function reads the file, looks for the key and sets
         ; its associated value to the parameter value
         ; all of the file data are loaded, otherwise a part of the file would be lost
         [read-all (λ(config-input-port)
                     (let download ()
                       (let ([definition-line (read config-input-port)])
                         (when (not
                                (eof-object? definition-line))
                           (hash-set! config-data
                                      (car definition-line)
                                      (if (eq? search-token
                                               (car definition-line))
                                          (begin0
                                            new-value
                                            (set! changed #t))
                                          (cadr definition-line)))
                           (download)))))]
         ; once the value changed, his function writes the file to its storage
         [write-all (λ(config-output-port)
                      (hash-for-each config-data
                                     (λ(k v)
                                       ; output-values is used for quoting strings
                                       (let ([output-value (if (string? v)
                                                               (~a "\"" v "\"")
                                                               v)])
                                         (display
                                          (~a "("
                                              k
                                              " "
                                              output-value
                                              ")\n")
                                          config-output-port)))))])

    ; reads and updates config
    (call-with-input-file
        (local-config-file-name)
      read-all
      #:mode 'text)

    (when debugger-on
      (display config-data)
      (display (~a "\nchanged: " changed)))

    (when changed
    ; saves new config
    (call-with-output-file
        (local-config-file-name)
      write-all
      #:mode 'text
      #:exists 'replace)

      changed)))


(define (test-edit-config)
  (edit-config "show-summary" #f))


(read-config (local-config-file-name))

(define-config-param show-summary
  #:default-value #t)

(define-config-param
  destroy-after-usage
  #:default-value #t)


(define-config-param display-margins
  #:default-value 20)
  
(define default-spacing
  (display-margins)) ; display parameters which sets controls horiz and vert margins

(define-config-param main-icon
  #:default-value "/home/ml/Documents/boom/boom2.png")


(define-config-param standard-report-file
  #:default-value "crash.json")

(define default-report-file
  (standard-report-file)) ; where on disc the report to be displayed should be put, when no file location is provided in command line


(define-config-param display-left-margin
  #:default-value 5)

(define default-left-margin
  (display-left-margin))


(define-config-param display-top-margin
  #:default-value 5)

(define default-top-margin
  (display-top-margin))

(define-config-param display-line-spacing
  #:default-value 0.5)

(define default-line-spacing
  (display-line-spacing))


(define-config-param resources-icons
  #:default-value "resources/ico/")

(define icon-directory
  (resources-icons))

(define-config-param resources-tasks
  #:default-value "accomplishment/")

(define task-icons-dir
  (resources-tasks))


(define (icon-path file-name)
  (~a icon-directory file-name))


(define/contract (summary-status-changed current-status)
  (-> boolean? boolean?)
  
  (not
   (eq?
    (show-summary)
    current-status)))


(define (now->string [date-only? #t])
  (date->string
   (seconds->date
    (current-seconds))

   (not date-only?)))


; computes date string length for a given format
(define/contract (guess-date-format-len some-format)
  (-> 	
   (or/c 'american
         'chinese
         'german
         'indian
         'irish
         'iso-8601
         'rfc2822
         'julian)
   exact-nonnegative-integer?)

  (let ([top-format-backup (date-display-format)]) ; backs up environment variable for later restoration
    (date-display-format some-format)

    (let ([date-len (string-length
                     (now->string))])
      (date-display-format top-format-backup) ; restores environmental variable
      date-len)))


; alt string to be displayed when some icon can not be loaded
(define bitmap-default-alt
  (string #\😕))


; bitmap a bitmap% object
; kind : symbol to provided to drawing procedure as specified by racket/draw
; width : image width
(struct icon-info (bitmap kind width))


(define/contract (extract-type-symbol icon-file)
  (-> string?
      (or/c symbol? boolean?))
  
  (let ([ext (let* ([raw (path-get-extension icon-file)])
               (cond
                 [(boolean? raw) raw]
                 [(bytes=? raw #".jpeg") raw]
                 [(bytes=? raw #".jpg") #".jpeg"]
                 [(or (bytes=? raw #".tif")
                      (bytes=? raw #".tiff")
                      (bytes=? raw #".TIF")
                      (bytes=? raw #".TIFF")) #f] ; filters out unsupported format
                 [else raw]))])
    (if (boolean? ext)
        #f
        (string->symbol
         (bytes->string/locale
          (subbytes
           ext
           1))))))


; a general loader for default or user-defined icons
; icon file is expected to sit in resources/ico/ directory
; returns bitmap% or alt
(define (load-boom-icon icon-file [alt bitmap-default-alt])
  (let ([full-path (icon-path icon-file)]
        [type-symbol (extract-type-symbol icon-file)])

    (if (boolean? type-symbol)
        alt
        (if (file-exists? full-path)
            (with-handlers ([exn:fail? (λ(e) alt)])
              (let ([b (make-object bitmap%
                         (icon-path icon-file)
                         type-symbol)])
                (if (send b ok?)
                    (icon-info
                     b
                     type-symbol
                     (send b get-width))
                    alt)))
            alt))))
      


; dates format
; style = a symbole, that can be taken by (date-display-format)
; len = expected string length
; icon can be either icon-info or a string. Any other type fires an exception in time-display.rkt
(struct ddisp(style len icon))

(define (make-date-display-style style icon)
  (ddisp style
         (guess-date-format-len style)
         (load-boom-icon icon)))

(define default-dates
  (make-date-display-style 'indian "uk.png" ))


(define us-dates
  (make-date-display-style 'american "us.png"))


(define good-date-indicator
  (load-boom-icon "ballgreen.png"))

; icon to be displayed next to date value, when this value is the default instead of the report value, which couldn't be read
(define wrong-date-indicator
  (load-boom-icon "ballred.png"))

(provide
 bitmap-default-alt
 builder-aspect
 debugger-on
 default-dates
 default-left-margin
 default-line-spacing
 default-report-file
 default-spacing
 default-top-margin
 destroy-after-usage
 edit-config
 good-date-indicator
 main-icon
 now->string
 restart-log-date
 show-summary
 summary-status-changed
 task-icons-dir
 us-dates
 wrong-date-indicator

 (contract-out
  [load-boom-icon (->* (string?)
                       (string?)
                       (or/c icon-info? string?))])

 (struct-out ddisp)
 (struct-out icon-info))