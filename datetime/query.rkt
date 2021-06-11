#lang racket/base

(require racket/contract/base
         datetime/date
         datetime/private/math)

(provide/contract
 [last-day-of-month (-> exact-integer? (integer-in 1 12) date?)])
                        

(define (last-day-of-month year month)
  (date year month (days-in-month year month)))

(define (nth-wday-in-month year month wday n)
  (cond [(>= n 0)
         (define first-of-month (date year month))
         (define first-of-month-wday (date->wday first-of-month))
         (define diff (mod (- wday first-of-month-wday) 7))
         (date-add-days first-of-month (+ diff (* 7 (sub1 n))))]
        [else
         (define last (last-day-of-month year month))
         (define last-wday (date->wday last))
         (define diff
           (let ([d1 (- wday last-wday)])
             (cond [(zero? d1) 0]
                   [(> d1 0) (- d1 7)]
                   [else d1])))
         (date-add-days last (- diff (* 7 (sub1 (- n)))))]))

(define (next-wday d wday #:include-current? [include-current? #f])
  (next-or-prev-wday d wday #t include-current?))

(define (prev-wday d wday #:include-current? [include-current? #f])
  (next-or-prev-wday d wday #f include-current?))

(define (next-or-prev-wday d wday next? include-current?)
  (define d-wday (date->wday d))
  
  (cond [(and include-current?
              (= wday d-wday))
         d]
        [else
         (define-values (diff sign)
           (cond [next? (values (- d-wday wday) +)]
                 [else  (values (- wday d-wday) -)]))
         (date-add-days d
                        (sign
                         (cond [(>= diff 0) (- 7 diff)]
                               [else        (- diff)])))]))