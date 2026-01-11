#lang racket

(require (for-syntax syntax/parse))

(provide solve check)

(define-syntax (solve stx)
    (syntax-parse stx
                  [(_)
                   #:with parse (datum->syntax stx 'parse)
                   #:with part-1 (datum->syntax stx 'part-1)
                   #:with part-2 (datum->syntax stx 'part-2)
                   #'(command-line
                       #:args (filename)
                       (define input (string-trim (file->string filename)))
                       (define parsed (parse input))
                       (printf "Part #1: ~a~n" (or (part-1 parsed) "Not Solved"))
                       (printf "Part #2: ~a~n" (or (part-2 parsed) "Not Solved")))]))

(define-syntax (check stx)
    (syntax-parse stx
                  [(_ solver expected/input ...)
                   #:with line (datum->syntax stx (syntax-line stx))
                   #:with parse (datum->syntax stx 'parse)
                   #'(begin
                       (printf "[~a:~a]~n" 'solver line)
                       (let ()
                           (match-define (list expected input) expected/input)
                           (define actual (solver (parse input)))
                           (if (equal? actual expected)
                                (printf "    \033[32m✔ ~a == ~a~n\033[0m" expected actual)
                                (printf "    \033[31m⨯ ~a != ~a~n\033[0m" expected actual))
                           ) ... )]))
