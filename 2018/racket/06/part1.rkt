#!/usr/bin/env racket

#lang racket/base

(require racket/function
         racket/list
         "lib.rkt")

(define (process-input in)
  (time
   (define points (port->points in))
   (define boundaries (find-bounds points))
   (define point-areas (make-hash))
   (for*
       ([x (in-range (bounds-min-x boundaries)
                     (add1 (bounds-max-x boundaries)))]
        [y (in-range (bounds-min-y boundaries)
                     (add1 (bounds-max-y boundaries)))])
     (define closest-points (closest-point (point x y) points))
     (define closest-inner-points
       (filter (curry inner-point? boundaries) closest-points))

     (unless (or (> (length closest-points) 1)  ; ignore equidistant points
                 (empty? closest-inner-points)) ; ignore outer points
       (define p (car closest-inner-points))
       (hash-set! point-areas p
                  (add1 (hash-ref! point-areas p 0)))))
   (apply max (hash-values point-areas))))

(call-with-input-file "input.txt" process-input)
