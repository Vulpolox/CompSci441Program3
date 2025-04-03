#lang racket

(require data/either)
(provide bind extract safe-input->list)


;; alias for from-either which extracts the pertinent value from the either monad
;; signature: (either) -> any/c
(define (extract either-monad) (from-either either-monad))


;; bind function for either monad
;; signature: (either func) -> failure | (func)
(define (bind func either-monad) 
    (if (failure? either-monad)
        either-monad
        (func (extract either-monad)))
)


;; function that converts a string into a racket list
;; signature: (string) -> either
(define (input->list str) (read (open-input-string str)))


(define (safe-input->list str)
    ;; user can include an optional apostrophe preceeding their input list
    (define mod-str (if (equal? #\' (string-ref str 0)) (first (string-split str "'")) str))
    (with-handlers 

        ;; if the string passed to this function is not
        ;; a valid racket list, return a failure
        {[exn:fail? (lambda (e) 
                            (failure (format "ERROR: Invalid List Structure")))
        ]}

        ;; otherwise, return a success w/ the
        ;; pertainent value as the list representation of the string
        {if (string-contains? mod-str "(")
            (success (input->list mod-str))
            (failure (format "ERROR: Input Must Be a List~n   e.g. \"(sub (add (num 1) (num 2)) (num 7)\""))}
    )
)
