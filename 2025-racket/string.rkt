#lang racket

(provide string->slices
         string->vector
         string-blank?
         is-all-digits?)

(define (string->slices chunk-size string)
  (for/list ([chunk (in-slice chunk-size (string->list string))])
    (list->string chunk)))

(define (string->vector string)
  (for/vector ([char (in-string string)])
    char))

(define (string-blank? string)
  (string=? (string-trim string) ""))

(define (is-all-digits? string)
  (regexp-match? #px"^[0-9]+$" string))
