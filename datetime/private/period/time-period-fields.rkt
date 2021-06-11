#lang racket/base

(require racket/contract/base
         racket/match
         "../time/util.rkt")

(provide (all-defined-out))

;;;;
;; Units
(define time-units '(hours minutes seconds milliseconds microseconds nanoseconds))
(define time-unit/c (apply symbols time-units))

;;;;
;; Predicates
(define (time-period-fields-empty? x)
  (match x
    [(time-period-fields 0 0 0 0 0 0) #t]
    [_ #f]))

;;;;
;; Scaling and Negation
(define (time-period-fields-scale x f)
  (match-define (time-period-fields h m s ms μs n) x)
  (time-period-fields (* h f) (* m f) (* s f) (* ms f) (* μs f) (* n f)))

(define (time-period-fields-complement x)
  (match-define (time-period-fields h m s ms μs n) x)
  (time-period-fields (- h) (- m) (- s) (- ms) (- μs) (- n)))

;;;;
;; Dict-like
(define (time-period-fields-ref x u)
  (match u
    ['hours        (time-period-fields-hours x)]
    ['minutes      (time-period-fields-minutes x)]
    ['seconds      (time-period-fields-seconds x)]
    ['milliseconds (time-period-fields-milliseconds x)]
    ['microseconds (time-period-fields-microseconds x)]
    ['nanoseconds  (time-period-fields-nanoseconds x)]))

(define (time-period-fields-set x u v)
  (match-define (time-period-fields h m s ms μs n) x)
  
  (match u
    ['hours        (time-period-fields v m s ms μs n)]
    ['minutes      (time-period-fields h v s ms μs n)]
    ['seconds      (time-period-fields h m v ms μs n)]
    ['milliseconds (time-period-fields h m s v μs n)]
    ['microseconds (time-period-fields h m s ms v n)]
    ['nanoseconds  (time-period-fields h m s ms μs v)]))

;;;;
;; Arithmetic
(define (time-period-fields-add x y)
  (match-define (time-period-fields h1 m1 s1 ms1 μs1 n1) x)
  (match-define (time-period-fields h2 m2 s2 ms2 μs2 n2) y)
  (time-period-fields (+ h1 h2) (+ m1 m2) (+ s1 s2) (+ ms1 ms2) (+ μs1 μs2) (+ n1 n2)))

;;;;
;; Conversion to flat times
(define (time-period-fields->nanoseconds p)
  (match-define (time-period-fields h m s ms μs n) p)

  (+ (* h NS/HOUR)
     (* m NS/MINUTE)
     (* s NS/SECOND)
     (* ms NS/MILLI)
     (* μs NS/MICRO)
     n))

;;;;
;; Struct definition
(struct time-period-fields (hours minutes seconds milliseconds microseconds nanoseconds)
  #:transparent)

;;;;
;; Constants
(define empty-time-period-fields (time-period-fields 0 0 0 0 0 0))

