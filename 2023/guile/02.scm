#!/usr/bin/guile \
-e main -s
!#
(use-modules
  (ice-9 hash-table)
  (ice-9 match)
  (ice-9 peg)
  (ice-9 textual-ports)
  (srfi srfi-1)  ; list library
  (srfi srfi-9)) ; record types

(define *reference-colors*
  '((red . 12)
    (green . 13)
    (blue . 14)))

(define-peg-string-patterns
  "game-list <- game-entry* !.
game-entry <- game-id set-list NL?
game-id <-- game-prefix number PS
game-prefix < 'Game '
set-list <-- cube-set+ !SS
cube-set <-- cube-spec+ SS?
cube-spec <-- number SP color CS?
color <-- 'red'/'green'/'blue'
number <-- [1-9] [0-9]*
CS < ', '
SS < '; '
NL < '\n'
PS < ': '
SP < ' '")

(define-record-type <game>
  (make-game number cube-sets)
  game?
  (number game-number)
  (cube-sets game-cube-sets set-game-cube-sets!))

(define (tokenize-game-list input)
  (peg:tree (match-pattern game-list input)))

(define (parse-game-list ast)
  (match ast
    ((games ..1) (map parse-game games))
    (_ (error "Invalid game list" ast))))

(define (parse-game ast)
  (match ast
    ((game-id set-list) (make-game (parse-game-id game-id) (parse-set-list set-list)))
    (_ (error "Invalid game node" ast))))
  
(define (parse-game-id ast)
  (match ast
    (('game-id number) (parse-number number))
    (_ (error "Invalid game-id node" ast))))

(define (parse-number ast)
  (match ast
    (('number n) (string->number n))
    (_ (error "Invalid number node" ast))))

(define (parse-set-list ast)
  (match ast
    (('set-list cube-sets ..1) (map parse-cube-set cube-sets))
    (_ (error "Invalid set-list node" ast))))

(define (parse-cube-set ast)
  (match ast
    (('cube-set cube-specs ..1) (map parse-cube-spec cube-specs))
    (_ (error "Invalid cube-set node" ast))))

(define (parse-cube-spec ast)
  (match ast
    (('cube-spec number color) (cons (parse-color color) (parse-number number)))
    (_ (error "Invalid cube-spec node" ast))))

(define (parse-color ast)
  (match ast
    (('color "red") 'red)
    (('color "green") 'green)
    (('color "blue") 'blue)
    (_ (error "Invalid color node" ast))))

(define (possible-game? game reference)
  (define colors (map car reference))
  (define (possible-cube-set? cube-set)
    (define (possible-color-count? color)
      (<= (or (assq-ref cube-set color) 0) (assq-ref reference color)))
    (and-map possible-color-count? colors))
  (and-map possible-cube-set? (game-cube-sets game)))

(define (new-color-table)
  (define ht (make-hash-table 3))
  (hashq-set! ht 'red 0)
  (hashq-set! ht 'green 0)
  (hashq-set! ht 'blue 0)
  ht)

(define (min-possible-colors game)
  (fold (lambda (color result)
          (fold (lambda (cube-set result)
                  (define new-value
                    (max (hashq-ref result color 0)
                         (or (assq-ref cube-set color) 0)))
                  (hashq-set! result color new-value)
                  result)
                result
                (game-cube-sets game)))
        (new-color-table)
        '(red green blue)))

(define (game-power game)
  (apply * (hash-map->list (lambda (_ v) v) (min-possible-colors game))))

(define (part1 input)
  (let* ((games (parse-game-list (tokenize-game-list input)))
         (possible? (lambda (game) (possible-game? game *reference-colors*)))
         (possible-games (filter possible? games)))
    (apply + (map game-number possible-games))))

(define (part2 input)
  (let ((games (parse-game-list (tokenize-game-list input))))
    (apply + (map game-power games))))

(define (main args)
  (match args
    ((_ input-path)
     (call-with-input-file input-path
       (lambda (port)
         (define input (get-string-all port))
         (format #t "Part 1: ~a\n" (part1 input))
         (format #t "Part 2: ~a\n" (part2 input)))))
    (_ (error "Usage: 02.scm INPUT-PATH" args))))
