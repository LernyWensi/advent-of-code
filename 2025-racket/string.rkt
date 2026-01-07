#lang racket

(provide string->slices)

(define (string->slices chunk-size string)
    (for/list ([chunk (in-slice chunk-size (string->list string))])
              (list->string chunk)))
