#!/usr/bin/env racket

#lang racket/base

(require racket/function
         racket/list
         racket/port
         racket/vector
         "lib.rkt")

(define (process-input in)
  (time
   (define guards (make-hasheq))
   (define entries (sort (port->lines in) string<?))
   (record-log-entries entries (box guards))
   (define sleepiest (argmax guard-total (hash-values guards)))
   (define minutes (guard-mins sleepiest))
   (* (guard-id sleepiest)
      (vector-member (vector-argmax identity minutes) minutes))))

(call-with-input-file "input.txt" process-input)
