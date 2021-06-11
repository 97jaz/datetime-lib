#lang racket/base

(require racket/contract/base)

(provide/contract
 [current-clock         (parameter/c (-> rational?))]
 [current-posix-seconds (-> rational?)])

(define (current-posix-seconds)
  (/ (inexact->exact (current-inexact-milliseconds)) 1000))

(define current-clock
  (make-parameter current-posix-seconds))
