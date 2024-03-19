#lang racket

(require
  racket/date
  racket/gui
  config
  "misc.rkt"
  "logging.rkt")

; Hard-coded configuration : this module provides default values to all app-specific parameters, which can be specified in config file

; turns on/off dubegging in this module and the others
(define debugger-on #f)

(define product-name "Boom")

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


(define report-extension #".json")

(define-config-param main-icon
  #:default-value "/home/ml/Documents/boom/boom2.png")


(define-config-param standard-report-file
  #:default-value (string-join
                   (list "crash"
                         (bytes->string/locale report-extension))
                   ""))

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


(define default-head-font-face 'modern)

; font face for boom main window message
; In opposition to the other config parmaters, this one is not exported by boom-parameters module,
; because its value is checked
; see config-head-font-face
(define-config-param head-font-face
  #:default-value (symbol->string default-head-font-face))


; exports head font face, that has been read from config file
; if this value is incorrect, it is replaced by default setting
(define/contract (config-head-font-face)
  (-> (or/c symbol? string?))
  
  (let* ([file-value (head-font-face)]
         [type-checked-value (if (non-empty-string? file-value)
                                 file-value

                                 (if (symbol? file-value)
                                     file-value
                                     #f))]
         [family-from-file (λ(f)
                             (let ([tested-value (cond
                                                   [(non-empty-string? f) (string->symbol f)]
                                                   [(symbol? f) f]
                                                   [else #f])])
                               (values
                                (if (boolean? tested-value)
                                    #f
                                    (racket-font-family? tested-value))
                                tested-value)))]
         [face-from-file (λ(f)
                           (let ([tested-value (cond
                                                 [(non-empty-string? f) f]
                                                 [(symbol? f) (symbol->string f)]
                                                 [else #f])])
                             (values
                              (if (non-empty-string? f)
                                  (not
                                   (not-in-list tested-value
                                                (get-face-list)))
                                  #f)
                              tested-value)))])
    
    (let-values ([(family? tested-family) (family-from-file type-checked-value)]
                 [(face? tested-face) (face-from-file type-checked-value)])
         
      (if (neither family? face?)
          (begin
            (log-invalid-font-face file-value)
            default-head-font-face)
          (if family?
              tested-family
              tested-face)))))


(define default-head-font-size 12.0)

(define-config-param head-font-size
  #:default-value default-head-font-size)


(define/contract (config-head-font-size)
  (-> real?)
  
  (let ([file-value (head-font-size)]
        [font-size? (λ(value)
                      (if (real? value)
                          (and (> value 4.0)
                               (< value 60.0))
                          #f))])
    (if (font-size? file-value)
        file-value
        (begin
          (log-invalid-font-size file-value)
          default-head-font-size))))
    


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


; expects a path to some icon-file and checks if extension is supported
; as this procedure is for use at module level only, it does not check mime-type
; outputs a symbol, that is used by bitmap%
(define/contract (extract-type-symbol icon-file)
  (-> string?
      (or/c symbol?
            boolean?))
  
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
         (~a
          (bytes->string/locale
           (subbytes
            ext
            1))
          "/alpha")))))


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
 config-head-font-face
 config-head-font-size
 main-icon
 now->string
 product-name
 report-extension
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