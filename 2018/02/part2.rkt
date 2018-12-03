#!/usr/bin/env racket

#lang racket/base

(require racket/contract/base)

(module+ test
  (require rackunit))

;;; Provides

(provide
 (contract-out
  [shared-chars (-> string? string? string?)]
  [find-match (-> (listof string?) (or/c void? (cons/c string? string?)))]
  [fuzzy-equal? (-> string? string? boolean?)]))

;;; Implementation

(require racket/match
         racket/port
         racket/string)

(define (shared-chars x y)
  (define shared-chars-list
    (for/list ([xc (in-string x)]
               [yc (in-string y)]
               #:when (eq? xc yc))
      xc))
  (list->string shared-chars-list))

(module+ test
  (test-equal?
   "Empty strings have no shared chars"
   (shared-chars "" "") "")

  (test-equal?
   "Yield an empty string for inputs with no shared chars"
   (shared-chars "foo" "bar") "")

  (test-equal?
   "Yield shared chars for strings with chars in the same position"
   (shared-chars "bar" "car") "ar"))

(define (fuzzy-equal? x y)
  (define different-chars
    (for/fold ([acc 0])
              ([xc (in-string x)]
               [yc (in-string y)])
      #:final (> acc 1)
      (if (char=? xc yc) acc (add1 acc))))
  (<= different-chars 1))

(module+ test
  (test-eq?
   "Strings with no shared chars are not equal"
   (fuzzy-equal? "foo" "bar") #f)

  (test-eq?
   "Strings with more than one different char are not equal"
   (fuzzy-equal? "foo" "far") #f)

  (test-eq?
   "Identical string are equal"
   (fuzzy-equal? "foo" "foo") #t)

  (test-eq?
   "Strings with one different char in the same position are equal"
   (fuzzy-equal? "echo" "ecto") #t))

(define (find-match strings)
  (define tail strings)
  (let/ec return
    (for ([s1 (in-list strings)])
      (set! tail (cdr tail))
      (for ([s2 (in-list tail)])
        (when (fuzzy-equal? s1 s2)
          (return (cons s1 s2)))))))

(module+ test
  (test-eq?
   "An empty list has no matches"
   (find-match '()) (void))

  (test-eq?
   "A single item list has no matches"
   (find-match '("foo")) (void))

  (test-equal?
   "A list with no matches returns void"
   (find-match '("foo" "bar" "zee")) (void))

  (test-equal?
   "A list with at least one match returns the first match"
   (find-match '("foo" "bar" "too" "car")) (cons "foo" "too")))

(module+ main
  (call-with-input-file "input.txt"
    (λ (in)
      (define boxes (filter non-empty-string? (port->lines in)))
      (time (match (find-match boxes)
              [(cons x y) (shared-chars x y)]
              [_ (error "No matches found")])))))
