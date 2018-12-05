#!/usr/bin/env racket

#lang racket/base

(require racket/port
         "lib.rkt")

(define (process-input in)
  (time
   (define polymer (port->string in))
   (string-length (reduce-polymer polymer))))

(call-with-input-file "input.txt" process-input)
