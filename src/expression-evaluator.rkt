#lang racket

(require data/either)
(provide evaluate)

(define identifier-hash (make-immutable-hash))

(define (evaluate expr) (identity expr))