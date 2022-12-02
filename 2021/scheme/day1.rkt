#lang racket

(define (part1 input)
  (let loop ([depth 0] [prev (car input)] [ns (cdr input)])
    (if (null? ns)
      depth
      (let ([n (car ns)])
	(if (> n prev)
	  (loop (add1 depth) n (cdr ns))
	  (loop depth n (cdr ns)))))))

(define (process-file p)
  (let loop ([n (read p)])
    (if (eof-object? n)
      '()
      (cons n (loop (read p))))))

(define (precise-time thunk)
  (let ([start (current-inexact-monotonic-milliseconds)])
    (begin0
      (thunk)
      (printf "Elapsed: ~a\n" (- (current-inexact-monotonic-milliseconds) start)))))

(define (main . args)
  #;(when (and (null? args) (not (null? (command-line))))
    (set! args (cdr (command-line))))

  (when (not (= (length args) 1))
    (display "USAGE: day1 INPUT")
    (exit))

  (let* ([path (car args)] 
	 [input (call-with-input-file path process-file)])
    (printf "Part 1: ~a\n" (precise-time (lambda () (part1 input))))))

(main "../inputs/01.txt")

#;(cond
  [(petite?)
   (suppress-greeting #t)
   (scheme-start main)]
  [else (main)])
