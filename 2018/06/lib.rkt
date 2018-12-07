#lang racket/base

(require racket/contract/base
         racket/list
         racket/match)

(provide
 (struct-out point)
 (struct-out bounds)
 (contract-out
  [find-bounds (-> (listof point?) bounds?)]
  [closest-point (-> point? (listof point?) (listof point?))]
  [manhattan-distance (-> point? point? natural-number/c)]
  [sum-distance (-> point? (listof point?) natural-number/c)]
  [outer-point? (-> bounds? point? boolean?)]
  [inner-point? (-> bounds? point? boolean?)]
  [port->points (-> port? (listof point?))]))

(struct point (x y))

(struct bounds (min-x max-x min-y max-y))

(define (find-bounds points)
  (bounds (point-x (argmin point-x points))
          (point-x (argmax point-x points))
          (point-y (argmin point-y points))
          (point-y (argmax point-y points))))

(define (closest-point p1 points)
  (define distances (make-hasheq))
  (for ([p2 (in-list points)])
    (define dist (manhattan-distance p1 p2))
    (hash-set! distances dist
               (cons p2 (hash-ref! distances dist '()))))
  (define closest (apply min (hash-keys distances)))
  (hash-ref distances closest))

(define (manhattan-distance p1 p2)
  (+ (abs (- (point-x p1) (point-x p2)))
     (abs (- (point-y p1) (point-y p2)))))

(define (sum-distance p1 points)
  (for/sum ([p2 (in-list points)])
    (manhattan-distance p1 p2)))

(define (outer-point? bounds p)
  (or (= (point-x p) (bounds-min-x bounds))
      (= (point-x p) (bounds-max-x bounds))
      (= (point-y p) (bounds-min-y bounds))
      (= (point-y p) (bounds-max-y bounds))))

(define (inner-point? bounds p)
  (not (outer-point? bounds p)))

(define (port->points in)
  (for/list ([line (in-lines in)])
    (match (regexp-match #px"([1-9]\\d*), ([1-9]\\d*)" line)
      [(list _ x y) (point (string->number x) (string->number y))]
      [_ (error "Invalid input format!")])))
