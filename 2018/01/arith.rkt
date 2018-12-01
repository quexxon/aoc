#lang racket/base

(provide read read-syntax)

(require racket/match
         racket/port)

(define (read in)
  (syntax->datum (read-syntax #f in)))

(define (read-syntax src in)
  (define input (port->string in))
  (define clean-input (regexp-replace* #px"\\s+" input ""))

  (define (parse-expr datum s)
    (match (regexp-match #px"^([+-])(\\d+)(.*)$" s)
      [(list _ op n tail)
       (define next-datum
         (list (string->symbol op)
               datum
               (string->number n)))
       (parse-expr next-datum tail)]
      [_ datum]))

  (define root-datum
    `(module arith-mod racket
       (time ,(parse-expr 0 clean-input))))

  (datum->syntax #f root-datum))
