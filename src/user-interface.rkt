#lang racket

(require data/either)


;; renaming of the id function for the (data/either) either monad
;; for my own convenience
;; signature: (either) -> any/c
(define (id either-monad) (from-either either-monad))


;; function that converts a string into a racket list
;; signature: (string) -> either
(define (input->list str) (read (open-input-string str)))

(define (safe-input->list str)
    (with-handlers 

        ;; if the string passed to this function is not
        ;; a valid racket list, return a failure
        {[exn:fail? (lambda (e) 
                            (failure "Error: invalid list structure"))
        ]}

        ;; otherwise, return a success w/ the
        ;; pertainent value as the list representation of the string
        {success (input->list str)}
    )
)

(safe-input->list "(hello world)")
(safe-input->list "(hello")
