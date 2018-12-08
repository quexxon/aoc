#!/usr/bin/env racket

#lang racket/base

(require racket/match
         racket/set
         "lib.rkt")

(define (process-input in)
  (time
   (define dg
     (for/fold ([dg (digraph (seteq) (hasheq) 0)])
               ([line (in-lines in)])
       (match (regexp-match #px"^.+([A-Z]).+([A-Z])" line)
         [(list _ from to)
          (add-edge dg (string-ref from 0) (string-ref to 0))]
         [_ (error "Invalid format!")])))
   (list->string (topological-order dg))))

(call-with-input-file "input.txt" process-input)
