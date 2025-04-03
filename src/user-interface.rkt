#lang racket

(require "./expression-evaluator.rkt")
(require "./utils.rkt")
(require data/either)

(provide execution-loop)


;; function for use in the execution loop; keeps calling 'func' until user indicates
;; they don't want to continue (reused from the parser program)
;; signature: (func, [string]) -> (func) or void
(define (continue? func state [message "Continue [any key], Exit [e]"])
    (printf "~n~a~n---~n   >>>" message)
    (define input (string-trim (read-line)))

    (if [nor (equal? input "e")
             (equal? input "E")]
        (func state)
        (begin
            (printf "---~n")
            'EXIT
        ) 
    )
)


(define (execution-loop state)
    (printf "~n---~nENTER EXPRESSION~n   >>>")
    (define user-input (string-trim (read-line)))
    (define expression (safe-input->list user-input))         ;; expression is an either monad
    (define expression-and-state                              ;; either w/ bundled state and expr
        (if
            (success? expression)
            (success (list (extract expression) state))
            (failure (list (extract expression) state))
        )
    )
    (define result-and-state (evaluate expression-and-state)) ;; either result or error message
    (let
        ([result (first (extract result-and-state))]
         [new-state (second (extract result-and-state))]
        )
        (begin
            (printf "~n---~n   ~a~n---~n" result)              ;; print the result/error message
            (continue? execution-loop new-state)               ;; ask the user if they want to cont
        )
    )
)
