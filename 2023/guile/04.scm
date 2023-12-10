#!/usr/bin/guile \
-e main -s
!#
(use-modules
  (ice-9 match)
  (ice-9 peg)
  (ice-9 textual-ports)
  (srfi srfi-1)
  (srfi srfi-9)
  (srfi srfi-26)
  (srfi srfi-69))

;; Card type ------------------------------------------------------------------

(define-record-type <card>
  (make-card id winners numbers)
  card?
  (id card-id)
  (winners card-winners)
  (numbers card-numbers))

(define (string->card s)
  (parse-card (peg:tree (match-pattern card s))))

(define (winning-number? c n)
  (set-has? (card-winners c) n))

(define (count-winning-numbers c)
  (define (accum n sum)
    (if (winning-number? c n) (1+ sum) sum))
  (fold accum 0 (card-numbers c)))

(define (card-score c)
  (define (score winning-numbers)
    (define count (length winning-numbers))
    (cond 
      ((zero? count) 0)
      (else 
       (let loop ((score 1) (count (1- count)))
         (cond 
           ((zero? count) score)
           (else (loop (* score 2) (1- count))))))))
  (score (filter (cute winning-number? c <>) (card-numbers c))))

;; Card string parser ---------------------------------------------------------

(define-peg-string-patterns
"card <-- id int-set SPACE BAR int-set
int-set <-- (SPACE integer)+
integer <-- ('0' ![0-9]) / ([1-9] [0-9]*)
id <-- CARD SPACE integer COLON
BAR < '|'
COLON < ':'
SPACE < ' '+
CARD < 'Card'")

(define (parse-integer ast)
  (match ast
    (('integer n) (string->number n))
    (_ (error "Failed to parse integer" ast))))

(define (parse-int-set ast)
  (match ast
    (('int-set ns ...) (map parse-integer ns))
    (_ (error "Failed to parse int-set" ast))))

(define (parse-id ast)
  (match ast
    (('id n) (parse-integer n))
    (_ (error "Failed to parse id" ast))))

(define (parse-card ast)
  (match ast
    (('card id winners numbers)
     (make-card (parse-id id)
                (list->set (parse-int-set winners))
                (parse-int-set numbers)))
    (_ (error "Failed to parse card" ast))))

;; Basic set type -------------------------------------------------------------

(define (list->set xs)
  (let ((alist (map (cut cons <> #t) xs)))
    (alist->hash-table alist)))

(define (set-add! set val)
  (hash-table-set! set val #t))

(define set-has? hash-table-exists?)

;; Day 4 ----------------------------------------------------------------------

(define (part1 input-path)
  (call-with-input-file input-path
    (lambda (port)
      (let loop ((line (get-line port))
                 (scores '()))
        (if (eof-object? line)
          (apply + scores)
          (loop (get-line port)
                (cons (card-score (string->card line)) scores)))))))

(define (part2 input-path)
  (define cards (make-hash-table))
  (define (inc-card-counts index count inc)
    (let loop ((i (1+ index)) (iter count))
      (when (> iter 0)
        (hash-table-update!/default cards i (cut + <> inc) 1)
        (loop (1+ i) (1- iter)))))
  (define (process-card c)
    (inc-card-counts (card-id c)
                     (count-winning-numbers c)
                     (hash-table-ref cards (card-id c))))
  (call-with-input-file input-path
    (lambda (port)
      (let loop ((line (get-line port)))
        (if (eof-object? line)
          (apply + (hash-table-values cards))
          (let ((c (string->card line)))
            (when (not (hash-table-exists? cards (card-id c)))
              (hash-table-set! cards (card-id c) 1))
            (process-card c)
            (loop (get-line port))))))))

(define (main args)
  (match args
    ((_ input-path)
     (format #t "Part 1: ~a\n" (part1 input-path))
     (format #t "Part 2: ~a\n" (part2 input-path)))
    (_ (error "Usage: 04.scm INPUT-PATH" args))))
