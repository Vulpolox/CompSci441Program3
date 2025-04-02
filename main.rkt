#lang racket

(require "./src/user-interface.rkt")

(define state (make-immutable-hash))
(execution-loop state)