#lang racket

(require "advent-of-code.rkt"
         "string.rkt"
         "vector.rkt")

(define (parse input)
  (list->vector (map string->vector (string-split input "\n"))))

(define (part-1 input)
  (for*/sum ([(row y) (in-indexed (in-vector input))]
             [(glyph x) (in-indexed (in-vector row))]
             #:when (char=? glyph #\@)
             [adjacents-cells (in-value (vector-2d-adjacents input x y))]
             #:when (< (vector-count (curry char=? #\@) adjacents-cells) 4))
            1))

(define (part-2 input)
  (let loop ([removed-rolls-total 0])
    (define rolls-to-remove
      (for*/list ([(row y) (in-indexed (in-vector input))]
                  [(glyph x) (in-indexed (in-vector row))]
                  #:when (char=? glyph #\@)
                  [adjacents-cells (in-value (vector-2d-adjacents input x y))]
                  #:when (< (vector-count (curry char=? #\@) adjacents-cells) 4))
        (list x y)))
    (if (null? rolls-to-remove)
        removed-rolls-total
        (begin
          (for ([roll (in-list rolls-to-remove)])
            (match-define (list x y) roll)
            (vector-2d-set! input x y #\x))
          (loop (+ removed-rolls-total (length rolls-to-remove)))))))

(module+ main
  (solve))

(module+ test
  (check part-1
         `(13 ,(string-join '("..@@.@@@@." "@@@.@.@.@@"
                                           "@@@@@.@.@@"
                                           "@.@@@@..@."
                                           "@@.@@@@.@@"
                                           ".@@@@@@@.@"
                                           ".@.@.@.@@@"
                                           "@.@@@.@@@@"
                                           ".@@@@@@@@."
                                           "@.@.@@@.@.")
                            "\n")))

  (check part-2
         `(43 ,(string-join '("..@@.@@@@." "@@@.@.@.@@"
                                           "@@@@@.@.@@"
                                           "@.@@@@..@."
                                           "@@.@@@@.@@"
                                           ".@@@@@@@.@"
                                           ".@.@.@.@@@"
                                           "@.@@@.@@@@"
                                           ".@@@@@@@@."
                                           "@.@.@@@.@.")
                            "\n"))))
