#!/usr/bin/guile \
-e main -s
!#
(use-modules
  (ice-9 textual-ports)
  (srfi srfi-1))

(define *digit-words*
  '(("one" . #\1)
    ("two" . #\2)
    ("three" . #\3)
    ("four" . #\4)
    ("five" . #\5)
    ("six" . #\6)
    ("seven" . #\7)
    ("eight" . #\8)
    ("nine" . #\9)))

(define (string-rcontains s1 s2)
  (let loop ((prev-index  #f) (index (string-contains s1 s2)))
    (if (eq? index #f)
      prev-index
      (loop index (string-contains s1 s2 (+ index 1))))))

(define (string-contains-any str strings)
  (let* ((f (lambda (s result)
              (let ((index (string-contains str s)))
                (if index
                  (cons (cons index s) result)
                  result))))
          (matches (fold f '() strings))
          (sorted (sort matches (lambda (x y) (< (car x) (car y))))))
    (if (null? sorted)
      #f
      (car sorted))))

(define (string-rcontains-any str strings)
  (let* ((f (lambda (s result)
              (let ((index (string-rcontains str s)))
                (if index
                  (cons (cons index s) result)
                  result))))
          (matches (fold f '() strings))
          (sorted (sort matches (lambda (x y) (> (car x) (car y))))))
    (if (null? sorted)
      #f
      (car sorted))))

(define (first-digit str)
  (string-ref str (string-index str char-numeric?)))

(define (first-digit-rev str)
  (define char-i (string-index str char-numeric?))
  (define word-match (string-contains-any str (map car *digit-words*)))
  (cond
    ((eq? char-i #f) (assoc-ref *digit-words* (cdr word-match)))
    ((eq? word-match #f) (string-ref str char-i))
    (else (if (< char-i (car word-match))
            (string-ref str char-i)
            (assoc-ref *digit-words* (cdr word-match))))))

(define (last-digit str)
  (string-ref str (string-rindex str char-numeric?)))

(define (last-digit-rev str)
  (define char-i (string-rindex str char-numeric?))
  (define word-match (string-rcontains-any str (map car *digit-words*)))
  (cond
    ((eq? char-i #f) (assoc-ref *digit-words* (cdr word-match)))
    ((eq? word-match #f) (string-ref str char-i))
    (else (if (> char-i (car word-match))
            (string-ref str char-i)
            (assoc-ref *digit-words* (cdr word-match))))))

(define (line-value line)
  (string->number (string (first-digit line) (last-digit line))))

(define (line-value-rev line)
  (string->number (string (first-digit-rev line) (last-digit-rev line))))

(define (parse-file input-path parse-line)
  (call-with-input-file input-path
    (lambda (port)
      (let loop ((line (get-line port)) (result 0))
        (if (eof-object? line)
          result
          (loop (get-line port) (+ result (parse-line line))))))))

(define (part1 input-path)
  (parse-file input-path line-value))

(define (part2 input-path)
  (parse-file input-path line-value-rev))

(define (main args)
  (let ((input-path (cadr args)))
    (format #t "Part 1: ~a\n" (part1 input-path))
    (format #t "Part 2: ~a\n" (part2 input-path))))
