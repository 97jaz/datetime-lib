#lang racket/base

(provide (all-defined-out))

(define (div+mod x y)
  (define-values (quot rem) (quotient/remainder x y))
  (if (< (bitwise-xor rem y) 0)
      (values (sub1 quot) (+ rem y))
      (values quot rem)))

(define (div x y)
  (define rem (remainder x y))
  (if (< (bitwise-xor rem y) 0)
      (sub1 (quotient x y))
      (quotient x y)))

(define (mod x y)
  (define rem (remainder x y))
  (if (< (bitwise-xor rem y) 0)
      (+ rem y)
      rem))

(define (mod1 x y)
  (- y (mod (- y x) y)))
