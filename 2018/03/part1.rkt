#!/usr/bin/env racket

#lang racket/base

(require racket/set
         "lib.rkt")

(call-with-input-file "input.txt"
  (λ (in)
    (time
     (define visited-points (mutable-seteq))
     (define overlap-points (mutable-seteq))
     (for ([line (in-lines in)])
       (define c (parse-claim line))
       (record-claim c (box visited-points) (box overlap-points)))
     (set-count overlap-points))))
