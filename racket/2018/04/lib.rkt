#lang racket/base

(require racket/contract/base
         racket/match)

(provide
 (struct-out guard))

(provide
 (contract-out
  [record-log-entries (-> (listof string?) (box/c hash?) void?)]))

(struct guard (id total mins) #:mutable)

(define (record-log-entries entries guards-bx)
  (define guards (unbox guards-bx))
  (define grd #f)
  (define asleep 0)
  (for ([entry (in-list entries)])
    (match (regexp-match log-entry-regexp entry)
      [(list _ _ "Guard" id)
       (set! grd (hash-ref! guards (string->number id)
                            (guard (string->number id) 0 (make-vector 60))))]
      [(list _ m "falls" _)
       (set! asleep (string->number m))]
      [(list _ m "wakes" _)
       (define awake (string->number m))
       (set-guard-total! grd (+ (guard-total grd) (- awake asleep)))
       (define mins (guard-mins grd))
       (for ([i (in-range asleep awake)])
         (vector-set! mins i (add1 (vector-ref mins i))))]
      [_ (error "Invalid entry format")])))

(define log-entry-regexp
  #px"^\\[[^:]+:(\\d+)\\] (\\w+) (?:#(\\d+))?")
