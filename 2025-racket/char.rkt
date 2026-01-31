#lang racket

(provide char->number)

(define (char->number char)
    (- (char->integer char)
       (char->integer #\0)))
