#lang racket/base

(require racket/contract/base
         racket/match)

(provide (all-defined-out))

;;;;
;; Units
(define date-units '(years months weeks days))
(define date-unit/c (apply symbols date-units))

;;;;
;; Predicates
(define (date-period-fields-empty? x)
  (match x
    [(date-period-fields 0 0 0 0) #t]
    [_ #f]))

;;;;
;; Scaling and Negation
(define (date-period-fields-scale x f)
  (match-define (date-period-fields y m w d) x)
  (date-period-fields (* y f) (* m f) (* w f) (* d f)))

(define (date-period-fields-complement x)
  (match-define (date-period-fields y m w d) x)
  (date-period-fields (- y) (- m) (- w) (- d)))

;;;;
;; Dict-like
(define (date-period-fields-ref x u)
  (match u
    ['years  (date-period-fields-years x)]
    ['months (date-period-fields-months x)]
    ['weeks  (date-period-fields-weeks x)]
    ['days   (date-period-fields-days x)]))

(define (date-period-fields-set x u n)
  (match-define (date-period-fields y m w d) x)
  (match u
    ['years  (date-period-fields n m w d)]
    ['months (date-period-fields y n w d)]
    ['weeks  (date-period-fields y m n d)]
    ['days   (date-period-fields y m w n)]))

;;;;
;; Arithmetic
(define (date-period-fields-add x y)
  (match-define (date-period-fields y1 m1 w1 d1) x)
  (match-define (date-period-fields y2 m2 w2 d2) y)
  (date-period-fields (+ y1 y2) (+ m1 m2) (+ w1 w2) (+ d1 d2)))

;;;;
;; Struct definition
(struct date-period-fields (years months weeks days) #:transparent)

;;;;
;; Constants
(define empty-date-period-fields (date-period-fields 0 0 0 0))
