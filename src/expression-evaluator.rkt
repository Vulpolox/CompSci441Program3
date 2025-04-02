#lang racket

(require data/either)
(require "./utils.rkt" "./bindings.rkt")
(provide evaluate)

;; signature: either<((expr | result | exn-message) state)> 
;; -> either<((expr | result | exn-message) state)>
(define (evaluate expr-and-state)
    (define expr (first (extract expr-and-state)))
    (define state (second (extract expr-and-state)))
    (define current-func (if (empty? expr) 'empty (first expr)))
    (define current-arguments (if (empty? expr) '() (rest expr)))
    (define correct-arity (get-arity current-func))

    (printf "~nLENGTH CURRENT ARGUMENTS: ~a~n" (length current-arguments))

    (cond
        ;; if evaluate is called on an expression that
        ;; has already errored out, return itself
        [(failure? expr-and-state) (identity (expr-and-state))]

        ;; if evaluate is called on an empty expression
        [(equal? current-func 'empty)
         (failure (list "ERROR: Tried to Evaluate Empty Expression" state))
        ]

        ;; if the function pulled from the expression is not in
        ;; '(num id add sub mult div assign remove define)
        [(is-invalid? current-func) 
         (failure (list (format "ERROR: Not A Function -> ~a" current-func) state))
        ]

        ;; special check for if the arity is correct if 'define is pulled
        ;; as it can have either 1 or 2 arguments
        [(and (is-define? current-func)
               (nor (= 1 (length current-arguments))
                    (= 2 (length current-arguments))
               )
         )
         (failure (list (format "ERROR: Arity Mismatch for \'define\': Got ~a Arguments, Expected 1 or 2"
                                (length current-arguments)
                        ) 
                    state
                  )
         )
        ]

        ;; if the function is called with the incorrect amount of arguments
        [(not (= correct-arity (length current-arguments)))
         (failure (list (format "ERROR: Arity Mismatch for \'~a\': Got ~a Arguments, Expected ~a"
                                current-func
                                (length current-arguments)
                                correct-arity
                        )
                    state
                  )
         ) 
        ]

        ;; if the function pulled is an operator
        [(is-op? current-func)
         (cond
            [#t #f]
         )
        ]

    )
)