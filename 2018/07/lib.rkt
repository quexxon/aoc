#lang racket/base

(require racket/contract/base)

(provide
 (struct-out digraph)
 (contract-out
  [make-stack (->* () #:rest (cons/c any/c (listof any/c)) mpair?)]
  [stack-push! (-> mpair? any/c void?)]
  [stack-pop! (-> mpair? any)]
  [stack-peek (-> mpair? any)]
  [stack->list (-> mpair? (listof any/c))]
  [add-edge (-> digraph? any/c any/c digraph?)]
  [topological-order (-> digraph? (listof any/c))]))

(require racket/function
         racket/list
         racket/set
         racket/undefined)

;;; Mutable Stack --------------------------------------------------------------

(define (make-stack . lst)
  (cond
    [(empty? lst) (mcons undefined (list))]
    [else (mcons (car lst) (cdr lst))]))

(define (stack-push! stack x)
  (unless (eq? (mcar stack) undefined)
    (set-mcdr! stack (cons (mcar stack) (mcdr stack))))
  (set-mcar! stack x))

(define (stack-pop! stack)
  (cond
    [(empty? (mcdr stack))
     (define top (mcar stack))
     (set-mcar! stack undefined)
     top]
    [else
     (define top (mcar stack))
     (set-mcar! stack (car (mcdr stack)))
     (set-mcdr! stack (cdr (mcdr stack)))
     top]))

(define (stack-peek stack)
  (mcar stack))

(define (stack->list stack)
  (cond
    [(eq? (mcar stack) undefined) (mcdr stack)]
    [else (cons (mcar stack) (mcdr stack))]))

;;; Directed Acyclic Graph -----------------------------------------------------

(struct digraph (vertices edges edge-count))

(define (add-edge dg from to)
  (define vertices
    (set-union (digraph-vertices dg) (seteq from to)))
  (define edges
    (hash-update (digraph-edges dg)
                 from
                 (curry cons to)
                 (list)))
  (define edge-count (add1 (digraph-edge-count dg)))
  (digraph vertices edges edge-count))

(define (topological-order dg)
  (define visited (mutable-seteq))
  (define stack (make-stack))
  (define vertex-list
    (sort (set->list (digraph-vertices dg)) char>?))
  (define (loop from)
    (define edges
      (hash-ref (digraph-edges dg) from (list)))
    (set-add! visited from)
    (for ([to (in-list edges)]
          #:unless (set-member? visited to))
      (loop to))
    (stack-push! stack from))
  (for ([from (in-list vertex-list)]
        #:unless (set-member? visited from))
    (loop from))
  (stack->list stack))
