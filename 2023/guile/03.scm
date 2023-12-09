#!/usr/bin/guile \
-e main -s
!#
(use-modules
  (ice-9 match)
  (ice-9 peg)
  (ice-9 textual-ports)
  (srfi srfi-1)
  (srfi srfi-9)
  (srfi srfi-11))

(define-peg-string-patterns
"pnum <-- [1-9] [0-9]*
psym <-- !'.' .
num-or-sym <- pnum / psym")

(define-record-type <pnum>
  (make-pnum value start end)
  pnum?
  (value pnum:value)
  (start pnum:start)
  (end pnum:end))

(define-record-type <psym>
  (make-psym value x)
  psym?
  (value psym:value)
  (x psym:x))

(define (parse-num-or-sym value)
  (match value
    ((('pnum n) start end) (make-pnum (string->number n) start end))
    ((('psym v) start _) (make-psym v start))
    (_ (error "Unexpected value" value))))

(define (parse-line line)
  (let loop ((offset 0)
             (result (search-for-pattern num-or-sym line))
             (matches '()))
    (if (not (peg-record? result))
      (map parse-num-or-sym matches)
      (let ((next-offset (+ offset (peg:end result))))
        (loop next-offset
              (search-for-pattern num-or-sym (string-drop line next-offset))
              (cons (list (peg:tree result)
                          (+ offset (peg:start result))
                          next-offset)
                    matches))))))

(define (adjacent? val sym)
  (let ((x (psym:x sym)))
    (if (and (pnum? val)
             (>= x (- (pnum:start val) 1))
             (<= x (pnum:end val)))
      val
      #f)))

(define (add-adjacent! vals sym ht)
  (define (maybe-add! val)
    (when (adjacent? val sym)
      (hashq-set! ht val #t)))
  (for-each maybe-add! vals))

(define (count-adjacent vals sym)
  (define (accum-sum val sum)
    (if (adjacent? val sym) (1+ sum) sum))
  (fold accum-sum 0 vals))

(define (parse-file input-path)
  (call-with-input-file input-path
    (lambda (port)
      (let loop ((line (get-line port))
                 (result '()))
        (if (eof-object? line)
          result
          (loop (get-line port) (cons (parse-line line) result)))))))

(define (add-gears! gears prev-line line next-line)
  (define (add-gear! value)
    (define adj-nums (make-hash-table))
    (when (and (psym? value) (string=? (psym:value value) "*"))
      (add-adjacent! prev-line value adj-nums)
      (add-adjacent! line value adj-nums)
      (add-adjacent! next-line value adj-nums)
      (when (= (hash-count (const #t) adj-nums) 2)
        (let* ((accum-product (λ (num _ product) (* product (pnum:value num))))
               (gear-ratio (hash-fold accum-product 1 adj-nums)))
          (hashq-set! gears value gear-ratio)))))
  (for-each add-gear! line))

(define (part1 input-path)
  (define adj-nums (make-hash-table))
  (let loop ((lines (parse-file input-path))
             (prev-line '()))
    (if (null? lines)
      (apply + (hash-map->list (lambda (k _) (pnum:value k)) adj-nums))
      (let* ((line (car lines))
             (psyms (filter psym? line))
             (prev-psyms (filter psym? prev-line)))
        (for-each (lambda (sym) (add-adjacent! prev-line sym adj-nums)) psyms)
        (for-each (lambda (sym) (add-adjacent! line sym adj-nums)) psyms)
        (for-each (lambda (sym) (add-adjacent! line sym adj-nums)) prev-psyms)
        (loop (cdr lines) line)))))

(define (part2 input-path)
  (define lines (parse-file input-path))
  (define gears (make-hash-table))
  (let loop ((prev-line '())
             (line (car lines))
             (lines (cdr lines)))
    (if (null? line)
      (apply + (hash-map->list (λ (_ val) val) gears))
      (let-values (((next-line lines)
                    (if (null? lines)
                      (values '() '())
                      (values (car lines) (cdr lines)))))
        (add-gears! gears prev-line line next-line)
        (loop line next-line lines)))))

(define (main args)
  (match args
    ((_ input-path)
     (format #t "Part 1: ~a\n" (part1 input-path))
     (format #t "Part 2: ~a\n" (part2 input-path)))
    (_ (error "Usage: 03.scm INPUT-PATH" args))))
