#!/usr/bin/env racket

#lang racket/base

(module+ test
  (require rackunit))

;;; Provides

(require racket/contract/base)

(provide
 (contract-out
  [box-values (-> string?
                  (values natural-number/c natural-number/c))]
  [checksum   (-> (listof string?)
                  natural-number/c)]))

;;; Implementation

(require racket/match
         racket/port
         racket/string)

(define (box-values box-id)
  (define char-counts (make-hasheq))
  (define-values (twos threes)
    (for/fold ([twos 0] [threes 0])
              ([char (in-string box-id)])
      (define count (add1 (hash-ref char-counts char 0)))
      (hash-set! char-counts char count)
      (match count
        [2 (values (add1 twos) threes)]
        [3 (values (sub1 twos) (add1 threes))]
        [4 (values twos (sub1 threes))]
        [_ (values twos threes)])))
  (values twos threes))

(module+ test
  (test-case
      "No letters appear 2 or 3 times"
    (define-values (twos threes) (box-values "abc"))
    (check-eq? twos 0)
    (check-eq? threes 0))

  (test-case
      "1 letter appears 2 times; 1 letter appears 3 times"
    (define-values (twos threes) (box-values "ababa"))
    (check-eq? twos 1)
    (check-eq? threes 1))

  (test-case
      "1 letter appears 2 times"
    (define-values (twos threes) (box-values "abb"))
    (check-eq? twos 1)
    (check-eq? threes 0))

  (test-case
      "1 letter appears 3 times"
    (define-values (twos threes) (box-values "abbb"))
    (check-eq? twos 0)
    (check-eq? threes 1))

  (test-case
      "3 letters appear 2 times"
    (define-values (twos threes) (box-values "aabbcc"))
    (check-eq? twos 3)
    (check-eq? threes 0))

  (test-case
      "3 letters appear 3 times"
    (define-values (twos threes) (box-values "abcabcabc"))
    (check-eq? twos 0)
    (check-eq? threes 3))

  (test-case
      "3 letters appear 2 times; 2 letters appear 3 times"
    (define-values (twos threes) (box-values "abbcaddcaeec"))
    (check-eq? twos 3)
    (check-eq? threes 2)))

(define (checksum boxes)
  (define-values (twos threes)
    (for/fold ([twos 0] [threes 0])
              ([box-id (in-list boxes)])
      (define-values (box-twos box-threes) (box-values box-id))
      (values (if (zero? box-twos) twos (add1 twos))
              (if (zero? box-threes) threes (add1 threes)))))
  (* twos threes))

(module+ test
  (test-eq? "No boxes" (checksum '()) 0)

  (test-eq? "One box" (checksum '("aabbb")) 1)

  (test-eq? "Two boxes" (checksum '("aabbb" "abcabc")) 2)

  (test-case
      "Many boxes"
    (define boxes
      '("abcdef" "bababc" "abbcde" "abcccd" "aabcdd" "abcdee" "ababab"))
    (check-eq? (checksum boxes) 12)))

(module+ main
  (call-with-input-file "input.txt"
    (λ (in)
      (define lines (filter non-empty-string? (port->lines in)))
      (time (checksum lines)))))
