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

        ;; === ERROR BASE CASES ======================

        ;; if evaluate is called on an expression that
        ;; has already errored out, return itself
        [(failure? expr-and-state) (identity expr-and-state)]

        ;; if the function pulled from the expression is not in
        ;; '(num id add sub mult div assign remove define)
        [(is-invalid? current-func) 
         (failure (list (format "ERROR: Unrecognized Operation -> ~a" current-func) state))
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
        [(nor (= correct-arity (length current-arguments))
              (equal? current-func 'define)
         )  
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

        ;; id function
        [(is-id? current-func)
            (let*
                ([identifier (first current-arguments)]
                 [value (hash-ref state identifier 'unbound)]
                )
                (cond
                    [(number? identifier) 
                     (failure (list (format "ERROR: ID Contract Violation: Expected identifier?, Got ~a" identifier) state))
                    ]
                    [(list? identifier) 
                     (failure (list (format "ERROR: ID Contract Violation: Expected identifier?, Got '~a" identifier) state))
                    ]
                    [(is-procedure? identifier)
                     (failure (list (format "ERROR: ID Contract Violation: Expected identifier?, Got #<procedure:~a>" identifier) state))
                    ]
                    [(not (char-alphabetic? (string-ref (symbol->string identifier) 0)))
                     (failure (list (format "ERROR: ID Naming Convention Violated: First Char Not Alplhabetic -> '~a'" identifier) state))
                    ]
                    [(ormap id-naming-predicate (string->list (symbol->string identifier)))
                     (failure (list (format "ERROR: ID Naming Convention Violated: ID Can Only Contain Alphanumeric Chars, '-', and '_' -> '~a'" identifier) state))
                    ]
                    [(equal? 'unbound value)
                     (failure (list (format "ERROR: Tried Accessing Unbound Identifier -> ~a" identifier) state))
                    ]
                    [(equal? 'undefined value)
                     (failure (list (format "ERROR: Identifier Accessed Before Being Assigned a Value -> ~a = 'undefined" identifier) state))
                    ]
                    [else (success (list value state))]
                )
            )
        ]

        ;; assign function
        [(is-assign? current-func)
            (let*
                ([identifier (first current-arguments)]
                 [expr (evaluate (success (list (second current-arguments) state)))]
                 [value (hash-ref state identifier 'unbound)]
                )
                (cond
                    [(failure? expr) expr]
                    [(number? identifier)
                     (failure (list (format "ERROR: ASSIGN Contract Violation: Expected identifier?, Got ~a" identifier) state))
                    ]
                    [(list? identifier)
                     (failure (list (format "ERROR: ASSIGN Contract Violation: Expected identifier?, Got '~a" identifier) state))
                    ]
                    [(is-procedure? identifier)
                     (failure (list (format "ERROR: ASSIGN Contract Violation: Expected identifier?, Got #<procedure:~a>" identifier) state))
                    ]
                    [(equal? value 'unbound)
                     (failure (list (format "ERROR: Tried to Assign to Unbound Identifier -> ~a" identifier) state))
                    ]
                    [else
                     (success 
                        (list 
                            (format "~a -> ~a" (first (extract expr)) identifier) 
                            (hash-set state identifier (first (extract expr)))
                        )
                     )
                    ]
                )
            )
        ]

        ;; define function
        [(is-define? current-func)
            (let*
                ([identifier (first current-arguments)]
                 [expr (if (= 2 (length current-arguments))
                           (evaluate (success (list (second current-arguments) state))) 
                           'null
                       )
                 ]
                )
                (cond
                    [(failure? expr) expr]
                    [(number? identifier) 
                     (failure (list (format "ERROR: DEFINE Contract Violation: Expected identifier?, Got ~a" identifier) state))
                    ]
                    [(list? identifier) 
                     (failure (list (format "ERROR: DEFINE Contract Violation: Expected identifier?, Got '~a" identifier) state))
                    ]
                    [(is-procedure? identifier)
                     (failure (list (format "ERROR: DEFINE Contract Violation: Expected identifier, Got #<procedure:~a>" identifier) state))
                    ]
                    [(not (char-alphabetic? (string-ref (symbol->string identifier) 0)))
                     (failure (list (format "ERROR: ID Naming Convention Violated: First Char Not Alplhabetic -> '~a'" identifier) state))
                    ]
                    [(ormap id-naming-predicate (string->list (symbol->string identifier)))
                     (failure (list (format "ERROR: ID Naming Convention Violated: ID Can Only Contain Alphanumeric Chars, '-', and '_' -> '~a'" identifier) state))
                    ]
                    [(hash-has-key? state identifier)
                     (failure (list (format "ERROR: ID Already Defined -> ~a = ~a" identifier (hash-ref state identifier)) state))
                    ]
                    [(equal? expr 'null)
                     (success (list (format "'undefined -> ~a" identifier) (hash-set state identifier 'undefined)))
                    ]
                    [(success? expr)
                     (success (list 
                                (format "~a -> ~a" (first (from-either expr)) identifier)
                                (hash-set state identifier (first (from-either expr)))
                              )
                     )
                    ]
                )
            )
        ]

        ;; remove function
        [(is-remove? current-func)
            (let*
                ([identifier (first current-arguments)]
                 [value (hash-ref state identifier 'unbound)]
                )
                (cond
                    [(number? identifier) 
                     (failure (list (format "ERROR: REMOVE Contract Violation: Expected identifier?, Got ~a" identifier) state))
                    ]
                    [(list? identifier) 
                     (failure (list (format "ERROR: REMOVE Contract Violation: Expected identifier?, Got '~a" identifier) state))
                    ]
                    [(is-procedure? identifier)
                     (failure (list (format "ERROR: REMOVE Contract Violation: Expected identifier?, Got #<procedure:~a>" identifier) state))
                    ]
                    [(equal? value 'unbound)
                     (failure (list (format "ERROR: Tried to Remove Unbound Identifier -> ~a" identifier) state))
                    ]
                    [else (success (list (format "REMOVED ~a" identifier) (hash-remove state identifier)))]
                )
            )
        ]


        ;; === OTHER BASE CASE ======================

        [(is-num? current-func)
         (num (first current-arguments) state)
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

(define (is-procedure? x)
    (list-contains? (list 'num 'id 'add 'sub 'div 'mult 'assign 'define 'remove) x)
)

(define (id-naming-predicate chr)
    (nor
        (char-numeric? chr)
        (char-alphabetic? chr)
        (equal? #\_ chr)
        (equal? #\- chr)
    )
)