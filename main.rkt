#lang racket

(require "./src/user-interface.rkt")

(define state (make-immutable-hash '((i . 4))))
(execution-loop state)