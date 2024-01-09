#lang racket
; module in charge of executing the restart command

(require
  syntax/identifier
  "strings-manager.rkt"
  "misc.rkt")


; not defined with contract. Firing exceptions is unwanted here, because we may want to use this procedure at preventing errors
(define (list-of-strings? tested)
  (let ([status (when (list? tested)
                  (andmap string? tested))])
    (if (boolean? status)
        status
        #f)))
        


(struct exe (command args)
  #:guard (λ(c a name)
            (when (or
                   ((λ(tested)
                      (if (string? tested)
                          (not (non-empty-string? tested))
                          #t)) c)
                   (and
                    (if (boolean? a) a #t)
                    (not (string? a))
                    (not (list-of-strings? a))))

              (error "invalid exe description"))

            (values c (if (list? a)
                          (string-join a " ")
                          a))))


(define/contract (exe-to-string info)
  (-> exe? string?)

  (~a (exe-command info)
      " "
      (exe-args info)))


(define/contract (split-report-command command)
  (-> string?
      (or/c exe? boolean?))

  (if (non-empty-string? command)
      (let* ([data (string-split command)]
             [file-part (car data)]
             [args-list (if (pair? data)
                            (cdr data)
                            #f)])
        (exe file-part
             (string-join args-list " ")))
      #f))


; nr = not read
; nf = not found in file system
; nex = not an executable
; stc = invalid command
; fine = fine
(define status-list
  (list 'nr 'nf 'nex 'stx 'fine))
    
  
(define (status-messages)
  (make-hasheq
   (map (λ(code message)
          (cons code message))
        status-list
        (list "not in report"
              "executable not found"
              "command is not an executable file"
              "null character detected"
              "OK"))))

             
(define/contract (not-a-status? v)
  (-> any/c boolean?)

  (not-in-list v status-list))


(define (exe/c?)
  (-> or/c symbol? boolean?))


; status value: 'nr (= not found in report), 'nf (= not found on disk or at url), 'nex (not executable), 'fine
(struct caller(sys decoded status)
  #:guard (λ(sys-atom caller-data status-atom name)
            (when (not
                   (symbol? sys-atom))
              (raise-argument-error (identifier->string #'sys-atom)
                                    (identifier->string #'symbol?)
                                    1
                                    sys-atom))

            (when (and
                   (not (boolean? caller-data))
                   (not (exe? caller-data)))
              (raise-argument-error (identifier->string #'caller-data)
                                    "(or/c symbol? boolean?)"
                                    2
                                    caller-data))

            (when (not-a-status? status-atom)
              (raise-argument-error (identifier->string #'status-atom)
                                    (~a status-list)
                                    3
                                    status-atom))

            (values sys-atom caller-data status-atom)))


(define (windows? sys-token)
  (eq? 'windows sys-token))

; retrieves argument for restart action
; checks if file exists, is executable and permissions are allowed (in case of linux)
(define/contract (extract-caller report)
  (-> hash? caller?)

  (let* ([cmd-extraction (hash-ref report 'restart #f)]

         [cmd (if (boolean? cmd-extraction)
                  cmd-extraction
                  (split-report-command cmd-extraction))]

         [sys-id (system-type)]

         [windows-exe? (λ(f)
                         (let ([name-parts (string-split f ".")])
                           (if (= 1
                                  (length name-parts))
                               #f
                               (string-ci=? "exe"
                                            (cadr name-parts)))))]
                              
         [linux-exe? (λ(f)
                       (list? (member 'execute
                                      (with-handlers ([exn:fail? (λ(e) '())])
                                        (file-or-directory-permissions f)))))]
         
         [status (if (exe? cmd)
                     ((λ(f a)
                        (if (file-exists? f)
                            (if ((if (windows? sys-id)
                                     windows-exe?
                                     linux-exe?) f)
                                (if (and (string-no-nuls? f)
                                         (if (string? a)
                                             (string-no-nuls? a)
                                             #t))
                                    'fine
                                    'stx)
                                'nex)
                            'nf)) (exe-command cmd)
                                  (exe-args cmd))
                     'nr)])
    (caller sys-id cmd status)))


(define/contract (start-caller prog message-proc prompt-proc)
  (-> caller? procedure? procedure? void?)

  (let* ([full-command-string (exe-to-string
                               (caller-decoded prog))]
         [prompt-failure (λ(cmd why)
                           (message-proc
                            (format "Sorry, command [~a] can not be run. Following error was detected: ~a"
                                    full-command-string
                                    why)))])
    (if (eq? (caller-status prog) 'fine)
       (let* ([user-choice (prompt-proc (~a "We are going to restart your application, using command line:"
                                            full-command-string))]
              [approved (or (eq? user-choice 'ok)
                            (eq? user-choice 'yes))])
         (when approved
           (with-handlers ([exn:fail? (λ(e)
                                        (prompt-failure e))])
             (if (windows?
                  (system-type))

                 (system* 'exact
                          full-command-string)
           
                 (system* (exe-command (caller-decoded prog))
                          (exe-args (caller-decoded prog)))))))

       (prompt-failure (hash-ref
                        (status-messages)
                        (caller-status prog)
                        "unknown cause")))))


(provide
 extract-caller
 start-caller
 
 (struct-out caller))