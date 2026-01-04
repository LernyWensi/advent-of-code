#lang racket

(require (for-syntax syntax/parse))

(provide solve check)

(define (solve parse part-1 part-2)
    (command-line
        #:args (filename)
        (define input (string-trim (file->string filename)))
        (define parsed (parse input))
        (printf "Part #1: ~a~n" (or (part-1 parsed) "Not Solved"))
        (printf "Part #2: ~a~n" (or (part-2 parsed) "Not Solved"))))

(define-syntax (check stx)
    (syntax-parse stx
                  [(_ parser solver expected input)
                   #:with line (datum->syntax stx (syntax-line stx))
                   #'(let ()
                       (define actual (solver (parser input)))
                       (if (equal? actual expected)
                           (printf "[~a:~a] [X] ~a == ~a~n" 'solver line expected actual)
                           (printf "[~a:~a] [_] ~a != ~a~n" 'solver line expected actual)))]))
