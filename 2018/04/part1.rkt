#lang racket

(require gregor)

(struct guard-time (id total entries))

(struct log-entry (id asleep awake))

(define log-entry-regexp
  #px"^\\[([\\d-]+) ([\\d:]+)\\] (\\w+) (?:#(\\d+))?")

(define (parse-entry entry)
  (match (regexp-match log-entry-regexp entry)
    [(list _ d t "Guard" id) (log-entry (string->number id) #f #f)]
    [(list _ d t "wakes" _)  (log-entry #f (parse-time t "HH:mm") #f)]
    [(list _ d t "falls" _)  (log-entry #f #f (parse-time t "HH:mm") #f)]
    [_ (error "Invalid entry format")]))
