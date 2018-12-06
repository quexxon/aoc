#!/usr/bin/env racket

#lang racket/base

(require racket/list
         racket/port
         racket/set)

(define (numbers-from-file in)
  (define (line->number ln)
    (define n (string->number ln))
    (and (number? n) n))
  (filter-map line->number (port->lines in)))

(define (find-repeated-sum ns)
  (define sum 0)
  (define known-sums (mutable-seteq 0))
  (for/last ([n (in-cycle ns)])
    (set! sum (+ sum n))
    #:final (set-member? known-sums sum)
    (set-add! known-sums sum)
    sum))

(module+ main
  (define (process-input in)
    (time
     (define numbers (numbers-from-file in))
     (find-repeated-sum numbers)))

  (call-with-input-file "part1.rkt" process-input))
