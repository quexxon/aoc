#!/usr/bin/env racket

#lang racket/base

(require racket/list
         racket/match)

(struct point (x y))

(struct bounds (min-x max-x min-y max-y))

(define (manhattan-dist p1 p2)
  (+ (abs (- (point-x p1) (point-x p2)))
     (abs (- (point-y p1) (point-y p2)))))

(define (port->points in)
  (for/list ([line (in-lines in)])
    (match (regexp-match #px"([1-9]\\d*), ([1-9]\\d*)" line)
      [(list _ x y) (point (string->number x) (string->number y))]
      [_ (error "Invalid input format!")])))

(define (min-point-x points)
  (point-x (argmin point-x points)))

(define (max-point-x points)
  (point-x (argmax point-x points)))

(define (min-point-y points)
  (point-y (argmin point-y points)))

(define (max-point-y points)
  (point-y (argmax point-y points)))

(define (outer-point? p bounds)
  (or (= (point-x p) (bounds-min-x bounds))
      (= (point-x p) (bounds-max-x bounds))
      (= (point-y p) (bounds-min-y bounds))
      (= (point-y p) (bounds-max-y bounds))))

(define (inner-point? p bounds)
  (not (outer-point? p bounds)))

(define (closest-point p1 points)
  (define distances (make-hasheq))
  (for ([p2 (in-list points)])
    (define dist (manhattan-dist p1 p2))
    (hash-set! distances dist
               (cons p2 (hash-ref! distances dist '()))))
  (define closest (apply min (hash-keys distances)))
  (hash-ref distances closest))

(call-with-input-file "input.txt"
  (λ (in)
    (time
     (define points (port->points in))
     (define boundaries
       (bounds (min-point-x points)
               (max-point-x points)
               (min-point-y points)
               (max-point-y points)))
     (define closest (closest-point (point 1 4) points))
     (define point-areas (make-hash))
     (for*
         ([x (in-range (bounds-min-x boundaries)
                       (add1 (bounds-max-x boundaries)))]
          [y (in-range (bounds-min-y boundaries)
                       (add1 (bounds-max-y boundaries)))])
       (define closest (closest-point (point x y) points))
       (define closest-inner
         (filter (λ (p) (inner-point? p boundaries)) closest))
       (unless (or (> (length closest) 1)  ; ignore equidistant points
                   (empty? closest-inner)) ; ignore outer points
         (define p (car closest-inner))
         (hash-set! point-areas p
                    (add1 (hash-ref! point-areas p 0))))
       )
     (apply max (hash-values point-areas)))))
