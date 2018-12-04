#!/usr/bin/env racket

#lang racket/base

(require racket/set
         "lib.rkt")

(call-with-input-file "input.txt"
  (λ (in)
    (time
     (define visited-points-bx (box (mutable-seteq)))
     (define overlap-points-bx (box (mutable-seteq)))
     (for ([line (in-lines in)])
       (define c (parse-claim line))
       (record-claim c visited-points-bx overlap-points-bx))
     (set-count (unbox overlap-points-bx)))))
