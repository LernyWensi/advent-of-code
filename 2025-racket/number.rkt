#lang racket

(provide number->char
         inclusive-distance)

(define (number->char number)
    (integer->char (+ 48 number)))

(define (inclusive-distance a b)
    (+ 1 (abs (- a b))))
