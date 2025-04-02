#lang racket

(require data/either)
(provide (all-defined-out))

(define keyword-hash
  (make-immutable-hash
   '(
     ('num . (λ (n) (num n)))
     ('id . (λ (state i) (id state i)))
     ('add . (λ (n1 n2) (add n1 n2)))
     ('sub . (λ (n1 n2) (sub n1 n2)))
     ('div . (λ (n1 n2) (div n1 n2)))
     ('mult . (λ (n1 n2) (mult n1 n2)))
     ('assign . (λ (state i expr) (assign state i expr)))
     ('define . (λ (state id [val "undefined"]) (def state id val)))
     ('remove . (λ (state id) (rem id)))
    )
  )
)

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
        (not (or (is-id? symbol) (is-num? symbol) (is-op? symbol) (is-state-modifier? symbol)))
)


;; function for returning the arity of a keyword function
(define (get-arity symbol)
    (cond
        [(or (is-remove? symbol) (is-num? symbol) (is-id? symbol)) 1]
        [(or (is-op? symbol) (is-assign? symbol)) 2]
        [else -1]
    )
)


;; signature: (string) -> either
(define (num n)
    (cond 
        [(number? n) (success n)]
        [else (failure (format "ERROR: NUM Contract Violation: Expected (num <number>); Got (num ~n)" n))]
    )
)


(define (add n1 n2)
    (if
        (or (failure? (num n1)) (failure? (num n2)))
        (failure (format "ERROR: ADD Contract Violation: Expected (add <number> <number>); Got (add ~a ~a)" n1 n2))
        (success (+ n1 n2))
    )
)


(define (sub n1 n2)
    (if
        (or (failure? (num n1)) (failure? (num n2)))
        (failure (format "ERROR: SUB Contract Violation: Expected (sub <number> <number>); Got (sub ~a ~a)" n1 n2))
        (success (- n1 n2))
    )
)


(define (mult n1 n2)
    (if
        (or (failure? (num n1)) (failure? (num n2)))
        (failure (format "ERROR: MULT Contract Violation: Expected (mult <number> <number>); Got (mult ~a ~a)" n1 n2))
        (success (* n1 n2))
    )
)


(define (div n1 n2)
    (cond
        [(or (failure? (num n1)) (failure? (num n2)))
         (failure (format "ERROR: DIV Contract Violation: Expected (div <number> <number>); Got (div ~a ~a)" n1 n2))
        ]
        [(= n2 0) (format "ERROR: Divide By Zero: (div ~a ~a)" n1 n2)]
        [else (success (/ n1 n2))]
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