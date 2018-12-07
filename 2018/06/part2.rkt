#!/usr/bin/env racket

#lang racket/base

(require "lib.rkt")

(define (process-input in)
  (time
   (define points (port->points in))
   (define boundaries (find-bounds points))
   (for*/sum
       ([x (in-range (bounds-min-x boundaries)
                     (add1 (bounds-max-x boundaries)))]
        [y (in-range (bounds-min-y boundaries)
                     (add1 (bounds-max-y boundaries)))]
        #:when ((sum-distance (point x y) points) . < . 10000))
     1)))

(call-with-input-file "input.txt" process-input)
