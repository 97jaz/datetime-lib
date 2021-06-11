#lang racket/base

(require racket/contract/base
         "util.rkt")

(provide (all-defined-out))

(define (day-of-month/c y m)
  (integer-in 1 (days-in-month y m)))
