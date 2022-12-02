#!/usr/bin/env racket

#lang racket/base

(require racket/list
         racket/port
         "lib.rkt")

(define (process-input in)
  (time
   (define polymer (port->string in))
   (define alphabet
     (map integer->char
          (range (char->integer #\a)
                 (add1 (char->integer #\z)))))
   (define lengths
     (for/list ([c (in-list alphabet)])
       (string-length (reduce-polymer polymer c))))
   (car (sort lengths <))))

(call-with-input-file "input.txt" process-input)
