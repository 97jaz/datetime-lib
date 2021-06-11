#lang racket/base

(provide (all-defined-out))

(define NS/SECOND 1000000000)
(define NS/MINUTE (* NS/SECOND 60))
(define NS/HOUR (* NS/MINUTE 60))
(define NS/DAY (* NS/HOUR 24))

(define NS/MILLI 1000000)
(define NS/MICRO 1000)
