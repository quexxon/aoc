#!/usr/bin/env racket

#lang racket/base

(require racket/bool
         racket/port)

(define (invert-polarity c)
  (if (char-lower-case? c)
      (char-upcase c)
      (char-downcase c)))

(define (reduce-polymer polymer)
  (define prev #f)
  (define result '())
  (for ([c (in-string polymer)])
    (cond
      [(false? prev)
       (set! prev c)]
      [(char=? c (invert-polarity prev))
       (set! prev (and (pair? result) (car result)))
       (unless (null? result)
         (set! result (cdr result)))]
      [else (set! result (cons prev result))
            (set! prev c)]))
  (unless (or (false? prev) (char-whitespace? prev))
    (set! result (cons prev result)))
  (list->string (reverse result)))

(module+ main
  (define (process-input in)
    (time
     (define polymer (port->string in))
     (string-length (reduce-polymer polymer))))

  (call-with-input-file "input.txt" process-input))
