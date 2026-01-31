#lang racket

(require "advent-of-code.rkt"
         "char.rkt"
         "number.rkt")

(define (parse input)
    (string-split input "\n"))

(define (part-1 input)
    (for/fold ([total-joltage 0])
              ([bank input])
              (match-define (list bank-init ... bank-last) (map char->number (string->list bank)))
              (define-values (digit-1 digit-2)
                      (for/fold ([digit-1 -1]
                                 [digit-2 -1])
                                ([digit-current bank-init])
                                (cond [(< digit-1 digit-current) (values digit-current -1)]
                                      [(< digit-2 digit-current) (values digit-1 digit-current)]
                                      [else (values digit-1 digit-2)])))
              (define bank-joltage (+ (* digit-1 10) (max digit-2 bank-last)))
              (+ total-joltage bank-joltage)))

(define (part-2 input)
    (for/fold ([total-joltage 0])
              ([bank input])
              (define-values (bank-joltage _)
                  (for/fold ([bank-joltage 0]
                             [bank (map char->number (string->list bank))])
                            ([batteries-to-turn-on (in-range 11 -1 -1)])
                            (define-values (batteries-pool batteries-rest) (split-at bank (- (length bank) batteries-to-turn-on)))
                            (define highest-joltage-battery (apply max batteries-pool))
                            (define batteries-pool-rest (rest (member highest-joltage-battery bank)))
                            (define batteries-pool-joltage (* highest-joltage-battery (expt 10 batteries-to-turn-on)))
                            (values (+ bank-joltage batteries-pool-joltage) batteries-pool-rest)))
              (+ total-joltage bank-joltage)))

(module+ main (solve))

(module+ test
    (check part-1
           '(98 "987654321111111")
           '(89 "811111111111119")
           '(78 "234234234234278")
           '(92 "818181911112111")
           `(357 ,(string-join '("987654321111111" "811111111111119" "234234234234278" "818181911112111") "\n")))

    (check part-2
           '(987654321111 "987654321111111")
           '(811111111119 "811111111111119")
           '(434234234278 "234234234234278")
           '(888911112111 "818181911112111")
           `(3121910778619 ,(string-join '("987654321111111" "811111111111119" "234234234234278" "818181911112111") "\n"))))
