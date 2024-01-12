#lang racket

(require
  racket/date
  racket/gui)

; provides default values to all app-specific parameters, which can be specified in config file

; original screen size. Used for computing frames aspect ratio
(define builder-aspect
  (list 1920 1080))


(define restart-log-file "restart.txt")

; time formatting for restart diary
; this is not shown by ui and therefore needn't be localized
(define restart-log-date 'iso-8601)

; default for destroy after use
(define default-dau #t)

(define default-spacing 20) ; display parameters which sets controls horiz and vert margins
(define default-icon "/home/ml/Documents/boom/boom2.png")
(define default-report-file "crash.json") ; where on disc the report to be displayed should be put, when no file location is provided in command line
(define default-left-margin 5)
(define default-top-margin 5)
(define default-line-spacing 0.5)

(define icon-directory "resources/ico/")
(define task-icons-dir "accomplishment/")


(define (icon-path file-name)
  (~a icon-directory file-name))


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
 default-dates
 default-dau
 default-icon
 default-left-margin
 default-line-spacing
 default-report-file
 default-spacing
 default-top-margin
 good-date-indicator
 now->string
 restart-log-date
 task-icons-dir
 us-dates
 wrong-date-indicator

 (contract-out
  [load-boom-icon (->* (string?)
                       (string?)
                       (or/c icon-info? string?))])

 (struct-out ddisp)
 (struct-out icon-info))