#lang racket/base

(require racket/contract
         racket/format
         racket/match
         racket/serialize
         data/order
         "private/compare.rkt"
         "private/math.rkt"
         "private/date/contract.rkt"
         "private/date/util.rkt"

         (for-syntax racket/base
                     syntax/transformer))

;;;;
;; JDN conversion
;; http://aa.quae.nl/en/reken/juliaansedag.html#3_1
(define (date->jdn x)
  (match-define (Date y m d) x)
  (define c0 (div (- m 3) 12))
  (define x4 (+ y c0))
  (define-values (x3 x2) (div+mod x4 100))
  (define x1 (- m (* 12 c0) 3))
  (+ (div (* 146097 x3) 4)
     (div (* 36525 x2) 100)
     (div (+ (* 153 x1) 2) 5)
     d
     1721119))

;; http://aa.quae.nl/en/reken/juliaansedag.html#3_2
(define (jdn->date jdn)
  (define-values (x3 r3) (div+mod (+ (* 4 (- jdn 1721120)) 3) 146097))
  (define-values (x2 r2) (div+mod (+ (* 100 (div r3 4)) 99) 36525))
  (define-values (x1 r1) (div+mod (+ (* 5 (div r2 100)) 2) 153))
  (define d (+ (div r1 5) 1))
  (define c0 (div (+ x1 2) 12))
  (define y (+ (* 100 x3) x2 c0))
  (define m (+ x1 (- (* 12 c0)) 3))
  (Date y m d))

;;;;
;; Predicate
(define (date? x)
  (Date? x))

;;;;
;; Accessors
(define (date->year d)
  (Date-y d))

(define (date->month d)
  (Date-m d))

(define (date->day d)
  (Date-d d))
         
(define (date->wday x)
  (match-define (Date y m d) x)
  (ymd->wday y m d))

(define (date->iso-wday x)
  (mod1 (date->wday x) 7))

(define (date->yday x)
  (match-define (Date y m d) x)
  (+ d
     (if (leap-year? y)
         (vector-ref CUMULATIVE-MONTH-DAYS/LEAP (sub1 m))
         (vector-ref CUMULATIVE-MONTH-DAYS (sub1 m)))))

(define (date->quarter x)
  (add1 (quotient (sub1 (Date-m x)) 3)))

(define (date->iso-week+wyear x)
  (define yday (date->yday x))
  (define iso-wday (date->iso-wday x))
  (define y (Date-y x))
  (define w (quotient (+ yday (- iso-wday) 10)
                      7))
     
  (cond [(zero? w)
         (define y-1 (sub1 y))
         (values (iso-weeks-in-year y-1) y-1)]
        [(and (= w 53) (> w (iso-weeks-in-year y)))
         (values 1 (add1 y))]
        [else
         (values w y)]))

(define (date->iso-week x)
  (define-values (w y) (date->iso-week+wyear x))
  w)

(define (date->iso-wyear x)
  (define-values (w y) (date->iso-week+wyear x))
  y)

;;;;
;; Arithmetic
(define (date-add-years x n)
  (match-define (Date y m d) x)
  (define ny (+ y n))
  (define max-dom (days-in-month ny m))
  (Date ny m (if (<= d max-dom) d max-dom)))

(define (date-add-months x n)
  (match-define (Date y m d) x)
  (define ny (+ y (div (+ m n -1) 12)))
  (define nm (let ([v (mod1 (+ m n) 12)])
               (if (< v 0)
                   (+ 12 v)
                   v)))
  (define max-dom (days-in-month ny nm))
  (Date ny nm (if (<= d max-dom) d max-dom)))

(define (date-add-days x n)
  (jdn->date (+ (date->jdn x) n)))

;;;;
;; Difference
(define (date->proleptic-month x)
  (match-define (Date y m d) x)
  (sub1 (+ (* y 12) m)))

(define (date-months-between d1 d2)
  (define (f d) (+ (* (date->proleptic-month d) 32) (Date-d d)))
  (quotient (- (f d2) (f d1)) 32))

(define (date-days-between d1 d2)
  (- (date->jdn d2) (date->jdn d1)))

;;;;
;; ISO 8601 formatting
(define (date->iso8601 x)
  (match-define (Date y m d) x)
  (define (f n len) (~r n #:min-width len #:pad-string "0"))
  (format "~a-~a-~a" (f y 4) (f m 2) (f d 2)))

;;;;
;; gen:equal+hash
(define (date-equal-proc x y _)
  (match-define (Date y1 m1 d1) x)
  (match-define (Date y2 m2 d2) y)
  
  (and (= y1 y1) (= m1 m2) (= d1 d2)))

(define (date-hash-proc x _)
  (match-define (Date y m d) x)
  
  (+ (arithmetic-shift y 11)
     (arithmetic-shift m 6)
     d))

(define (date-hash2-proc x _)
  (match-define (Date y m d) x)
  
  (+ y
     (arithmetic-shift m 21)
     (arithmetic-shift d 27)))

;;;;
;; gen:custom-write
(define (date-write-proc x out mode)
  (if mode
      (fprintf out "#<date ~a>" (date->iso8601 x))
      (display (date->iso8601 x) out)))

;;;;
;; struct definition
(struct Date (y m d)
  #:methods gen:equal+hash
  [(define equal-proc date-equal-proc)
   (define hash-proc  date-hash-proc)
   (define hash2-proc date-hash2-proc)]
  
  #:methods gen:custom-write
  [(define write-proc date-write-proc)]

  #:property prop:serializable
  (make-serialize-info (λ (x)
                         (match-define (Date y m d) x)
                         (vector y m d))
                       #'deserialize-info:date
                       #f
                       (or (current-load-relative-directory)
                           (current-directory))))

(define deserialize-info:date
  (make-deserialize-info
   Date
   (λ () (error "date cannot have cycles"))))

(module+ deserialize-info
  (provide deserialize-info:date))

;;;;
;; Smart constructor
(define make-date
  (let ()
    (define (date y [m 1] [d 1])
      (Date y m d))
    date))

(define-module-boundary-contract
  date
  make-date
  (->i ([year exact-integer?])
       ([month (integer-in 1 12)]
        [day (year month) (day-of-month/c year month)])
       [d date?]))

(define-match-expander $date
  (syntax-rules ()
    [(_ y m d) (Date y m d)])
  (make-variable-like-transformer #'date))

;;;;
;; Comparison
(define (date-comparator x y)
  (match (- (Date-y x) (Date-y y))
    [(? negative?) '<]
    [(? positive?) '>]
    [_ (match (- (Date-m x) (Date-m y))
         [(? negative?) '<]
         [(? positive?) '>]
         [_ (match (- (Date-d x) (Date-d y))
              [(? negative?) '<]
              [(? zero?)     '=]
              [_             '>])])]))

(match-define (comparison date=? date<? date<=? date>? date>=? date-order)
  (build-comparison 'date-order date? date-comparator))

;;;;
;; Calendar query
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

(define (last-day-of-month year month)
  (make-date year month (days-in-month year month)))

(define (nth-wday-in-month year month wday n)
  (cond [(>= n 0)
         (define first-of-month (make-date year month 1))
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
                   [else 1])))
         (date-add-days last (- diff (* 7 (sub1 (- n)))))]))

(define (next-wday d wday #:include-current? [include-current? #f])
  (next-or-prev-wday d wday #t include-current?))

(define (prev-wday d wday #:include-current? [include-current? #f])
  (next-or-prev-wday d wday #f include-current?))

;;;;
;; Exports
(provide/contract
 [date? (-> any/c boolean?)]

 [date->year  (-> date? exact-integer?)]
 [date->month (-> date? (integer-in 1 12))]
 [date->day   (-> date? (integer-in 1 31))]
 
 [date->iso8601 (-> date? string?)]
 
 [date=?     (-> date? date? boolean?)]
 [date<?     (-> date? date? boolean?)]
 [date>?     (-> date? date? boolean?)]
 [date<=?    (-> date? date? boolean?)]
 [date>=?    (-> date? date? boolean?)]
 [date-order order?]

 [date->jdn (-> date? exact-integer?)]
 [jdn->date (-> exact-integer? date?)]

 [date->wday           (-> date? (integer-in 0 6))]
 [date->iso-wday       (-> date? (integer-in 1 7))]
 [date->yday           (-> date? (integer-in 1 366))]
 [date->quarter        (-> date? (integer-in 1 4))]
 [date->iso-week+wyear (-> date? (values (integer-in 1 53) exact-integer?))]
 [date->iso-week       (-> date? (integer-in 1 53))]
 [date->iso-wyear      (-> date? exact-integer?)]

 [date-add-years       (-> date? exact-integer? date?)]
 [date-add-months      (-> date? exact-integer? date?)]
 [date-add-days        (-> date? exact-integer? date?)]

 [date-months-between (-> date? date? exact-integer?)]
 [date-days-between   (-> date? date? exact-integer?)]

 [leap-year?        (-> exact-integer? boolean?)]
 [days-in-year      (-> exact-integer? (or/c 365 366))]
 [days-in-month     (-> exact-integer? (integer-in 1 12) (integer-in 28 31))]
 [iso-weeks-in-year (-> exact-integer? (or/c 52 53))]
 [last-day-of-month (-> exact-integer? (integer-in 1 12) date?)]
 [nth-wday-in-month (-> exact-integer? (integer-in 1 12) (integer-in 0 6) exact-integer?
                        date?)]
 [next-wday         (->* (date? (integer-in 0 6)) (#:include-current? boolean?) date?)]
 [prev-wday         (->* (date? (integer-in 0 6)) (#:include-current? boolean?) date?)])

(provide (rename-out [$date date]))
