#lang racket/base

(require "../math.rkt")

(provide (all-defined-out))

(define (leap-year? y)
  (and (zero? (remainder y 4))
       (or (not (zero? (remainder y 100)))
           (zero? (remainder y 400)))))

(define (days-in-year y)
  (if (leap-year? y) 366 365))

(define (days-in-month y m)
  (let ([delta (if (and (= m 2)
                        (leap-year? y))
                   1
                   0)])
    (+ (vector-ref DAYS-PER-MONTH m) delta)))

(define (ymd->wday y m d)
  ;; https://en.wikipedia.org/wiki/Determination_of_the_day_of_the_week#Implementation-dependent_methods
  (define t (vector 0 3 2 5 0 3 5 1 4 6 2 4))
  (define y1 (if (< m 3) (sub1 y) y))
  (mod (+ y1
          (div y1 4)
          (- (div y1 100))
          (div y1 400)
          (vector-ref t (sub1 m))
          d)
       7))

(define (iso-weeks-in-year y)
  (define w (ymd->wday y 1 1))

  (cond [(or (= w 4)
             (and (leap-year? y) (= w 3)))
         53]
        [else
         52]))

(define DAYS-PER-MONTH
  (vector 0 31 28 31 30 31 30 31 31 30 31 30 31))

(define CUMULATIVE-MONTH-DAYS
  (vector 0 31 59 90 120 151 181 212 243 273 304 334))

(define CUMULATIVE-MONTH-DAYS/LEAP
  (vector 0 31 60 91 121 152 182 213 244 274 305 335))
