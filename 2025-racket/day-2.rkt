#lang racket

(require "advent-of-code.rkt"
         "list.rkt"
         "string.rkt")

(struct id-range (min-bound max-bound) #:transparent)

(define (parse input)
    (for/list ([line (string-split input ",")])
              (match-define `(,min-bound ,max-bound) (map string->number (string-split line "-")))
              (id-range min-bound max-bound)))

(define (part-1 input)
    (for/fold ([invalid-ids-sum 0])
              ([an-id-range input])
              (match-define (id-range min-bound max-bound) an-id-range)
              (+ invalid-ids-sum
                 (for/fold ([sum 0])
                           ([number (in-range min-bound (add1 max-bound))])
                           (define number-digits (number->string number))
                           (define number-width (string-length number-digits))
                           (if (and (even? number-width)
                                    (all? equal? (string->slices (/ number-width 2) number-digits)))
                               (+ sum number) sum)))))

(define (part-2 input)
    (for/fold ([invalid-ids-sum 0])
              ([an-id-range input])
              (match-define (id-range min-bound max-bound) an-id-range)
              (+ invalid-ids-sum
                 (for/fold ([sum 0])
                           ([number (in-range min-bound (add1 max-bound))])
                           (define number-digits (number->string number))
                           (define number-width (string-length number-digits))
                           (for/fold ([sum-updated sum])
                                     ([chunk-size (in-range 1 (add1 number-width))]
                                      #:break (not (equal? sum-updated sum)))
                                     (if (and (zero? (modulo number-width chunk-size))
                                              (all? equal? (string->slices chunk-size number-digits) #:allow-single? #f))
                                         (+ sum number) sum))))))

(module+ main
         (solve parse part-1 part-2))

(module+ test
         (check parse part-1 33 "11-22")
         (check parse part-1 99 "95-115")
         (check parse part-1 1010 "998-1012")
         (check parse part-1 1188511885 "1188511880-1188511890")
         (check parse part-1 222222 "222220-222224")
         (check parse part-1 0 "1698522-1698528")
         (check parse part-1 446446 "446443-446449")
         (check parse part-1 38593859 "38593856-38593862")
         (check parse part-1 1227775554
                (string-join '("11-22" "95-115" "998-1012" "1188511880-1188511890" "222220-222224" "1698522-1698528" "446443-446449" "38593856-38593862") ","))

         (check parse part-2 33 "11-22")
         (check parse part-2 210 "95-115")
         (check parse part-2 2009 "998-1012")
         (check parse part-2 1188511885 "1188511880-1188511890")
         (check parse part-2 222222 "222220-222224")
         (check parse part-2 0 "1698522-1698528")
         (check parse part-2 446446 "446443-446449")
         (check parse part-2 38593859 "38593856-38593862")
         (check parse part-2 565656 "565653-565659")
         (check parse part-2 824824824 "824824821-824824827")
         (check parse part-2 2121212121 "2121212118-2121212124")
         (check parse part-2 4174379265
                (string-join '("11-22" "95-115" "998-1012" "1188511880-1188511890" "222220-222224" "1698522-1698528" "446443-446449" "38593856-38593862" "565653-565659" "824824821-824824827" "2121212118-2121212124") ",")))
