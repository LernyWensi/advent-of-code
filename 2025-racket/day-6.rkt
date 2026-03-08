#lang racket

(require "advent-of-code.rkt"
         "list.rkt"
         "string.rkt")

(define (calculate-total-sum operators operands)
  (for/fold ([sum 0])
            ([operator-string (in-list operators)]
             [numbers (in-slice (floor (/ (length operands) (length operators)))
                                (map string->number operands))])
    (define operator (if (equal? operator-string "*") * +))
    (+ sum (apply operator numbers))))

(define (parse input)
  (string-split input "\n"))

(define (part-1 input)
  (define grouped-tokens (map string-split input))
  (define-values (operands operators) (partition (curry ormap is-all-digits?) grouped-tokens))
  (calculate-total-sum (append* operators) (append* (transpose operands))))

(define (part-2 input)
  (define input-transposed (transpose (map (curryr string-split "") input)))
  (define input-grouped (group-by-divider (curry andmap string-blank?) input-transposed))
  (for/sum ([group input-grouped])
           (define condensed-group (map (curry filter (negate string-blank?)) group))
           (match-define (list (list digits-first ... operator) digits-rest ...) condensed-group)
           (apply (if (equal? operator "*") * +)
                  (map (compose string->number (curryr string-join ""))
                       (cons digits-first digits-rest)))))

(module+ main
  (solve))

(module+ test
  (check part-1
         `(4277556 ,(string-join
                     '("123 328  51 64 " " 45 64  387 23 " "  6 98  215 314" "*   +   *   +  ")
                     "\n")))

  (check part-2
         `(3263827 ,(string-join
                     '("123 328  51 64 " " 45 64  387 23 " "  6 98  215 314" "*   +   *   +  ")
                     "\n"))))
