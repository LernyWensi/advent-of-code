#lang racket

(provide all?)

(define (all? predicate? lst
              #:allow-empty? [allow-empty? #t]
              #:allow-single? [allow-single? #t])
    (match lst
           [(list) allow-empty?]
           [(list first) allow-single?]
           [(list first rest ...) (andmap (curry predicate? first) rest)]))
