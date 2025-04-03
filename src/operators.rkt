#lang racket

(require data/either)
(provide (all-defined-out))

;; simple boolean returning functions
(define (list-contains? lst e) (ormap (λ (element) (equal? e element)) lst))
(define (is-id? symbol) (equal? symbol 'id))
(define (is-num? symbol) (equal? symbol 'num))
(define (is-op? symbol) (list-contains? '(add sub mult div) symbol))
(define (is-state-modifier? symbol) (list-contains? (list 'assign 'define 'remove) symbol))
(define (is-assign? symbol) (equal? symbol 'assign))
(define (is-define? symbol) (equal? symbol 'define))
(define (is-remove? symbol) (equal? symbol 'remove))
(define (is-invalid? symbol) 
        (if
            (not (symbol? symbol))
            #t
            (not (or (is-id? symbol) (is-num? symbol) (is-op? symbol) (is-state-modifier? symbol)))
        )
        
)


;; function for returning the arity of a keyword function
(define (get-arity symbol)
    (cond
        [(or (is-remove? symbol) (is-num? symbol) (is-id? symbol)) 1]
        [(or (is-op? symbol) (is-assign? symbol)) 2]
        [else -1]
    )
)


(define (num n state)
    (cond 
        [(number? n) (success (list n state))]
        [else (failure (list (format "ERROR: NUM Contract Violation: Expected (num <number>); Got (num ~a)" n)
                             state
                       )
              )
        ]
    )
)


(define (op-template operator e1 e2 state)
    (let
        ([t1 (first (from-either e1))]
         [t2 (first (from-either e2))]
        )
        (success (list (operator t1 t2) state))
    )
)

(define add (λ (e1 e2 state) (op-template + e1 e2 state)))
(define sub (λ (e1 e2 state) (op-template - e1 e2 state)))
(define mult (λ (e1 e2 state) (op-template * e1 e2 state)))


(define (div e1 e2 state)
    (let
        ([dividend (first (from-either e1))]
         [divisor (first (from-either e2))]
        )
        (if 
            (= divisor 0)
            (failure (list (format "ERROR: Divide By Zero -> (div ~a ~a)" dividend divisor) state))
            (success (list (/ dividend divisor) state))
        )
    )
)


(define (id state i)
    (if
        (hash-has-key? state i)
        (success (hash-ref state i))
        (failure (format "ERROR: Unbound Identifier -> ~a" i))
    )
)


(define (assign state i expr)
    "todo"
)