(define (string-split str delim)
  (define in (open-input-string str))
  (let loop ((rv '()) (out (open-output-string)))
    (define c (read-char in))
    (cond ((eof-object? c)
           (reverse (cons (get-output-string out) rv)))
          ((char=? c delim)
           (loop (cons (get-output-string out) rv)
                 (open-output-string)))
          (else
           (write-char c out)
           (loop rv out)))))

(define source-file (cadr (command-line)))
(define basename (car (string-split source-file #\.)))

(generate-inspector-information #f)
(compile-file source-file)

(let ([boot-file (string-append basename ".boot")]
      [object-file (string-append basename ".so")])
  (make-boot-file boot-file '("petite") object-file))

