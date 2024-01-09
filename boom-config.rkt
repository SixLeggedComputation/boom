#lang racket

(require
  config
  "boom-parameters.rkt")

(read-config "config.txt")

(define-config-param cfg-dau
  #:default-value default-dau)


(provide
 cfg-dau)
