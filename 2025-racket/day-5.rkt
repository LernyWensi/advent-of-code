#lang racket

(require "advent-of-code.rkt"
         "number.rkt")

(define (parse input)
    (match-define (list fresh-ranges product-ids)
        (map (λ (block) (string-split block "\n"))
             (string-split input "\n\n")))
    (list
        (for/list ([id-range fresh-ranges])
                  (map string->number (string-split id-range "-")))
        (for/list ([id product-ids])
                  (string->number id))))

(define (part-1 input)
    (match-define (list fresh-ranges product-ids) input)
    (for/sum ([product-id product-ids]
              #:when (ormap (match-lambda [(list min max)
                                           (<= min product-id max)])
                            fresh-ranges))
             1))

(define (part-2 input)
  (match-define (list fresh-ranges-raw _) input)
  (define fresh-ranges-sorted (sort (remove-duplicates fresh-ranges-raw) < #:key first))
  (match-define (list min-initial max-initial) (first fresh-ranges-sorted))
  (for/fold ([fresh-ids-total 0]
             [min-current min-initial]
             [max-current max-initial]
             #:result (+ fresh-ids-total (inclusive-distance max-current min-current)))
            ([fresh-range (rest fresh-ranges-sorted)])
            (match-define (list min-next max-next) fresh-range)
            (if (<= min-next max-current)
                (values fresh-ids-total min-current (max max-current max-next))
                (values (+ fresh-ids-total (inclusive-distance max-current min-current)) min-next max-next))))

(module+ main (solve))

(module+ test
    (check part-1
           `(3 ,(string-join '("3-5"
                               "10-14"
                               "16-20"
                               "12-18"
                               ""
                               "1"
                               "5"
                               "8"
                               "11"
                               "17"
                               "32") "\n")))

    (check part-2
           `(14 ,(string-join '("3-5"
                                "10-14"
                                "16-20"
                                "12-18"
                                ""
                                "1"
                                "5"
                                "8"
                                "11"
                                "17"
                                "32") "\n"))))
