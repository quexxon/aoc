#lang racket

(require gregor)

(struct guard-time (id total entries))

(struct log-entry (id asleep awake) #:mutable)

(define log-entry-regexp
  #px"^\\[([\\d-]+) (\\d+):(\\d+)\\] (\\w+) (?:#(\\d+))?")

(define guard-log (make-hasheq))

(define (new-blank-log-entry) (log-entry #f #f #f))

(define (record-entry datestring hour update-entry)
  (define log-date
    (let ([d (parse-date datestring "yyyy-MM-dd")])
      (if (string=? hour "23") (+days d 1) d)))
  (define key (->posix log-date))
  (define entry (hash-ref! guard-log key new-blank-log-entry))
  (update-entry entry))

(define (parse-entry entry)
  (match (regexp-match log-entry-regexp entry)
    [(list _ d h _ "Guard" id)
     (record-entry d h
                   (λ (entry)
                     (set-log-entry-id! entry (string->number id))))]
    [(list _ d h m "wakes" _)
     (record-entry d h
                   (λ (entry)
                     (set-log-entry-awake! entry (string->number m))))]
    [(list _ d h m "falls" _)
     (record-entry d h
                   (λ (entry)
                     (set-log-entry-asleep! entry (string->number m))))]
    [_ (error "Invalid entry format")]))

(parse-entry "[1518-10-18 23:51] Guard #349 begins shift")

(call-with-input-file "input.txt"
  (λ (in)
    (for ([entry (in-lines in)])
      (parse-entry entry))))

(for ([entry (in-hash-values guard-log)])
  (printf "Guard: ~a, Asleep: ~a, Awake: ~a\n"
          (log-entry-id entry)
          (log-entry-asleep entry)
          (log-entry-awake entry)))
