#lang racket

(provide string->slices
         string->vector)

(define (string->slices chunk-size string)
    (for/list ([chunk (in-slice chunk-size (string->list string))])
              (list->string chunk)))

(define (string->vector string)
    (for/vector ([char (in-string string)])
                char))
