#lang racket

(require "advent-of-code.rkt")

(define (parse input)
    (for/list ([line (string-split input "\n")])
              (define delta (string->number (substring line 1)))
              (define sign (match (string-ref line 0)
                                  [#\L -]
                                  [#\R +]
                                  [direction (error "unknow direction: ~a; expected \"L\" or \"R\"" direction)]))
              (sign delta)))

(define (part-1 input)
    (for/fold ([dial 50]
               [counter 0]
               #:result counter)
              ([delta input])
              (define target (+ dial delta))
              (define dial-updated (modulo target 100))
              (define counter-updated (+ counter (if (zero? dial-updated) 1 0)))
              (values dial-updated counter-updated)))

(define (part-2 input)
    (define (count-rotations dial delta target)
        (define round (if (positive? delta) floor ceiling))
        (abs (- (round (/ dial 100))
                (round (/ target 100)))))

    (for/fold ([dial 50]
               [counter 0]
               #:result counter)
              ([delta input])
              (define target (+ dial delta))
              (define dial-updated (modulo target 100))
              (define counter-updated (+ counter (count-rotations dial delta target)))
              (values dial-updated counter-updated)))

(module+ main (solve))

(module+ test
        (check part-1
               `(0 ,(string-join '("L68") "\n"))
               `(0 ,(string-join '("L68" "L30") "\n"))
               `(1 ,(string-join '("L68" "L30" "R48") "\n"))
               `(1 ,(string-join '("L68" "L30" "R48" "L5") "\n"))
               `(1 ,(string-join '("L68" "L30" "R48" "L5" "R60") "\n"))
               `(2 ,(string-join '("L68" "L30" "R48" "L5" "R60" "L55") "\n"))
               `(2 ,(string-join '("L68" "L30" "R48" "L5" "R60" "L55" "L1") "\n"))
               `(3 ,(string-join '("L68" "L30" "R48" "L5" "R60" "L55" "L1" "L99") "\n"))
               `(3 ,(string-join '("L68" "L30" "R48" "L5" "R60" "L55" "L1" "L99" "R14") "\n"))
               `(3 ,(string-join '("L68" "L30" "R48" "L5" "R60" "L55" "L1" "L99" "R14" "L82") "\n")))

        (check part-2
               `(1 ,(string-join '("L68") "\n"))
               `(1 ,(string-join '("L68" "L30") "\n"))
               `(2 ,(string-join '("L68" "L30" "R48") "\n"))
               `(2 ,(string-join '("L68" "L30" "R48" "L5") "\n"))
               `(3 ,(string-join '("L68" "L30" "R48" "L5" "R60") "\n"))
               `(4 ,(string-join '("L68" "L30" "R48" "L5" "R60" "L55") "\n"))
               `(4 ,(string-join '("L68" "L30" "R48" "L5" "R60" "L55" "L1") "\n"))
               `(5 ,(string-join '("L68" "L30" "R48" "L5" "R60" "L55" "L1" "L99") "\n"))
               `(5 ,(string-join '("L68" "L30" "R48" "L5" "R60" "L55" "L1" "L99" "R14") "\n"))
               `(6 ,(string-join '("L68" "L30" "R48" "L5" "R60" "L55" "L1" "L99" "R14" "L82") "\n"))))
