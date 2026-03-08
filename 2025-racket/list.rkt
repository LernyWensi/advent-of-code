#lang racket

(provide all?
         transpose
         group-by-divider)

(define (all? predicate? lst #:allow-empty? [allow-empty? #t] #:allow-single? [allow-single? #t])
  (match lst
    [(list) allow-empty?]
    [(list first) (and allow-single? (predicate? first))]
    [(list first rest ...) (andmap (curry predicate? first) rest)]))

(define (transpose input)
  (apply map list input))

(define (group-by-divider divider? elements-list)
  (define (append-group groups group)
    (if (empty? group)
        groups
        (cons (reverse group) groups)))
  (for/fold ([all-groups '()]
             [current-group '()]
             #:result (append-group all-groups current-group))
            ([element elements-list])
    (if (divider? element)
        (if (empty? current-group)
            (values all-groups '())
            (values (append-group all-groups current-group) '()))
        (values all-groups (cons element current-group)))))
