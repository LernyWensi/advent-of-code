#lang racket

(provide number->char)

(define (number->char number)
    (integer->char (+ 48 number)))
