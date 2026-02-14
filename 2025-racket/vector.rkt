#lang racket

(provide vector-ref-maybe
         vector-2d-ref-maybe
         vector-2d-adjacents
         vector-2d-set/copy
         vector-2d-set!)

(define (vector-ref-maybe vector index)
    (define in-range? (and (>= index 0)
                           (< index (vector-length vector))))
    (if in-range? (vector-ref vector index) #f))

(define (vector-2d-ref-maybe vector-2d y x)
    (define row (vector-ref-maybe vector-2d y))
    (and row (vector-ref-maybe row x)))

(define (vector-2d-adjacents vector-2d pivot-x pivot-y)
    (for*/vector ([dy (in-inclusive-range -1 1)]
                  [dx (in-inclusive-range -1 1)]
                  #:unless (and (zero? dx)
                                (zero? dy))
                  [value (in-value (vector-2d-ref-maybe vector-2d
                                                        (+ pivot-y dy)
                                                        (+ pivot-x dx)))]
                  #:when value)
                 value))

(define (vector-2d-set/copy vector-2d x y value)
    (define row-current (vector-ref vector-2d y))
    (define row-updated (vector-set/copy row-current x value))
    (vector-set/copy vector-2d y row-updated))

(define (vector-2d-set! vector-2d x y value)
    (define row (vector-ref vector-2d y))
    (vector-set! row x value))
