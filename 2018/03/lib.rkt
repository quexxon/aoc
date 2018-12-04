#lang racket/base

(require racket/match
         racket/set)

(provide (struct-out claim))
(provide parse-claim record-claim overlap?)

(struct claim (id x y width height))

(define (parse-claim str)
  (match (regexp-match #px"#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)" str)
    [(list _ id x y width height)
     (claim (string->number id)
            (string->number x)
            (string->number y)
            (string->number width)
            (string->number height))]
    [_ (error "Invalid format")]))

(define (make-point x y)
  (+ (* x 1024) y))

(define (record-claim c visited-points-bx overlap-points-bx)
  (define visited-points (unbox visited-points-bx))
  (define overlap-points (unbox overlap-points-bx))
  (define x-start (add1 (claim-x c)))
  (define x-end (+ x-start (claim-width c)))
  (define y-start (add1 (claim-y c)))
  (define y-end (+ y-start (claim-height c)))
  (for* ([x (in-range x-start x-end)]
         [y (in-range y-start y-end)])
    (define point (make-point x y))
    (cond
      [(set-member? visited-points point)
       (set-add! overlap-points point)]
      [else (set-add! visited-points point)])))

(define (overlap? c overlap-points-bx)
  (define overlap-points (unbox overlap-points-bx))
  (define x-start (add1 (claim-x c)))
  (define x-end (+ x-start (claim-width c)))
  (define y-start (add1 (claim-y c)))
  (define y-end (+ y-start (claim-height c)))
  (for*/or ([x (in-range x-start x-end)]
            [y (in-range y-start y-end)])
    (define point (make-point x y))
    (set-member? overlap-points point)))
