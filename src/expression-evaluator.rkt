#lang racket

(require data/either)
(require "./utils.rkt" "./operators.rkt")
(provide evaluate)

;; signature: either<'((expr | result | exn-message) state)> 
;; -> either<'((expr | result | exn-message) state)>
(define (evaluate expr-and-state)
    (define expr (first (extract expr-and-state)))
    (define state (second (extract expr-and-state)))
    (define current-func (if (modified-empty? expr) 'empty (modified-car expr)))
    (define current-arguments (if (modified-empty? expr) '() (modified-cdr expr)))
    (define correct-arity (get-arity current-func))

    (cond

        ;; === ERROR HANDLING ======================

        ;; if evaluate is called on an expression that
        ;; has already errored out, return itself
        [(failure? expr-and-state) (identity (expr-and-state))]

        ;; if the function pulled from the expression is not in
        ;; '(num id add sub mult div assign remove define)
        [(is-invalid? current-func) 
         (failure (list (format "ERROR: Not A Function -> ~a" current-func) state))
        ]

        ;; if evaluate is called on an empty expression
        [(equal? current-func 'empty)
         (failure (list "ERROR: Tried to Evaluate Empty Expression" state))
        ]

        ;; special check for whether the arity is correct if 'define is pulled
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

        ;; === NUMERIC OPERATORS ======================

        [(is-op? current-func)
            (let
                ([e1 (evaluate (success (list (first current-arguments) state)))]
                 [e2 (evaluate (success (list (second current-arguments) state)))]
                )
                (cond
                    [(failure? e1) e1]
                    [(failure? e2) e2]
                    [(equal? current-func 'add) (add e1 e2 state)]
                    [(equal? current-func 'sub) (sub e1 e2 state)]
                    [(equal? current-func 'mult) (mult e1 e2 state)]
                    [(equal? current-func 'div) (div e1 e2 state)]
                )
            )
        ]

        ;; === STATE ACCESSORS/MUTATORS ======================

        ;; === BASE CASES ======================

        [(is-num? current-func)
         (num (first current-arguments) state)
        ;  (let 
        ;     ([result (num (first current-arguments))])
        ;     (if
        ;         (success? result)
        ;         (success (list (extract result) state))
        ;         (failure (list (extract result) state))
        ;     )
        ;  )
        ]
    )
)


;; === HELPER FUNCTIONS ====================

(define (modified-empty? x)
    (if (list? x) (empty? x) #f)
)

(define (modified-car x)
    (if (list? x) (car x) x)
)

(define (modified-cdr x)
    (if (list? x) (cdr x) '())
)