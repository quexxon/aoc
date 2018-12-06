#!/usr/bin/env racket

#lang racket/base

(require racket/function
         racket/list
         racket/port
         racket/vector
         "lib.rkt")

(define (most-freq-minute grd)
  (vector-argmax identity (guard-mins grd)))

(define (process-input in)
  (time
   (define guards (make-hasheq))
   (define entries (sort (port->lines in) string<?))
   (record-log-entries entries (box guards))
   (define grd (argmax most-freq-minute (hash-values guards)))
   (define minutes (guard-mins grd))
   (* (guard-id grd)
      (vector-member (vector-argmax identity minutes) minutes))))

(call-with-input-file "input.txt" process-input)
