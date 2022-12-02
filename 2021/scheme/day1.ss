#lang r6rs

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

(define (main . args)
  #;(when (and (null? args) (not (null? (command-line))))
    (set! args (cdr (command-line))))

  (when (not (= (length args) 1))
    (display "USAGE: day1 INPUT")
    (exit))

  (let* ([path (car args)] 
	 [input (call-with-input-file path process-file)])
    (format #t "Part 1: ~a\n" (time (part1 input)))))

(main "../inputs/01.txt")

#;(cond
  [(petite?)
   (suppress-greeting #t)
   (scheme-start main)]
  [else (main)])
