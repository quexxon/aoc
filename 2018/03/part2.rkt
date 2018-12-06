#!/usr/bin/env racket

#lang racket/base

(require racket/port
         racket/set
         "lib.rkt")

(define (process-input in)
  (time
   (define visited-points-bx (box (mutable-seteq)))
   (define overlap-points-bx (box (mutable-seteq)))
   (define claims (map parse-claim (port->lines in)))
   (for ([c (in-list claims)])
     (record-claim c visited-points-bx overlap-points-bx))
   (for/last ([c (in-list claims)])
     (define has-overlap (overlap? c overlap-points-bx))
     #:final (not has-overlap)
     (claim-id c))))

(call-with-input-file "input.txt" process-input)
