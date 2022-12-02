#lang racket/base

(require racket/bool
         racket/contract/base)

(provide
 (contract-out
  [invert-polarity (-> char? char?)]
  [reduce-polymer (->* (string?)
                       ((or/c char? false?))
                       string?)]))

(define (invert-polarity c)
  (if (char-lower-case? c)
      (char-upcase c)
      (char-downcase c)))

(define (reduce-polymer polymer [skip-char #f])
  (define prev '())
  (define result
    (for/fold ([prev '()])
              ([c (in-string polymer)])
      (cond
        [(and (char? skip-char)
              (char-ci=? c skip-char)) prev]
        [(null? prev) (cons c prev)]
        [(char=? c (invert-polarity (car prev))) (cdr prev)]
        [else (if (char-whitespace? c)
                  prev
                  (cons c prev))])))
  (list->string (reverse result)))
