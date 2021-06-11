#lang racket/base

(require racket/contract
         racket/format
         racket/generic
         racket/match
         (only-in racket/math
                  exact-floor exact-round)
         racket/serialize
         data/order
         "date.rkt"
         "time.rkt"
         "private/compare.rkt"
         "private/date/contract.rkt"
         "private/time/util.rkt"

         (for-syntax racket/base
                     syntax/transformer))

;;;;
;; gen:equal+hash
(define (datetime-equal-proc x y fn)
  (match-define (Datetime d1 t1) x)
  (match-define (Datetime d2 t2) y)
  (and (fn d1 d2)
       (fn t1 t2)))

(define (datetime-hash-proc x fn)
  (match-define (Datetime d t) x)
  (bitwise-xor (fn d) (fn t)))

;;;;
;; gen:custom-write
(define (datetime-write-proc x out mode)
  (if mode
      (fprintf out "#<datetime ~a>" (datetime->iso8601 x))
      (display (datetime->iso8601 x) out)))

;;;;
;; ISO 8601 formatting
(define (datetime->iso8601 x)
  (match-define (Datetime d t) x)
  (format "~aT~a"
          (date->iso8601 d)
          (time->iso8601 t)))

;;;;
;; Predicate
(define (datetime? x)
  (Datetime? x))

;;;;
;; Accessors
(define (datetime->date x)
  (Datetime-date x))

(define (datetime->time x)
  (Datetime-time x))

;;;;
;; Julian day conversion
(define (datetime->jd x)
  (match-define  (Datetime d t) x)
  (define jdn    (date->jdn d))
  (define day-ns (time->day-ns t))

  (+ (- jdn 1/2)
     (/ day-ns NS/DAY)))

(define (jd->datetime jd)
  (define jdn    (jd->jdn jd))
  (define d      (jdn->date jdn))
  (define day-ns (jd->day-ns jd))
  (define t      (day-ns->time day-ns))

  (Datetime d t))

(define (jd->jdn jd)
  (define lo (exact-floor jd))

  ;; math-class rounding: round up for >= 1/2
  (if (>= (- jd lo) 1/2)
      (add1 lo)
      lo))

(define (jd->day-ns jd)
  (define base (- jd 1/2))
  (define frac (- base (exact-floor base)))

  (exact-round (* frac NS/DAY)))

;;;;
;; Posix conversion
(define (datetime->posix x)
  (jd->posix (datetime->jd x)))

(define (posix->datetime posix)
  (jd->datetime (posix->jd posix)))

(define (jd->posix jd)
  (* 86400 (- jd (+ 2440587 1/2))))

(define (posix->jd posix)
  (+ (/ posix 86400) (+ 2440587 1/2)))

;;;;
;; Arithmetic
(define (datetime-add-months dt n)
  (match-define (Datetime d t) dt)
  (Datetime (date-add-months d n) t))

(define (datetime-add-days dt n)
  (match-define (Datetime d t) dt)
  (Datetime (date-add-days d n) t))

(define (datetime-add-nanoseconds dt ns)
  (jd->datetime
   (+ (datetime->jd dt)
      (/ ns NS/DAY))))

;;;;
;; Difference
(define (datetime-date-difference-end-date x y)
  (match-define (Datetime d1 t1) x)
  (match-define (Datetime d2 t2) y)

  (cond [(and (date>? d2 d1) (time<? t2 t1))
         (date-add-days d2 -1)]
        [(and (date<? d2 d1) (time>? t2 t1))
         (date-add-days d2 1)]
        [else
         d2]))

(define (datetime-months-between x y)
  (date-months-between (Datetime-date x)
                       (datetime-date-difference-end-date x y)))

(define (datetime-days-between x y)
  (date-days-between (Datetime-date x)
                     (datetime-date-difference-end-date x y)))

(define (datetime->jdns x)
  (exact-floor (* (datetime->jd x) NS/DAY)))

(define (datetime-nanoseconds-between x y)
  (- (datetime->jdns y) (datetime->jdns x)))

;;;;
;; Struct definition
(struct Datetime (date time)
  #:methods gen:equal+hash
  [(define equal-proc datetime-equal-proc)
   (define hash-proc  datetime-hash-proc)
   (define hash2-proc datetime-hash-proc)]

  #:methods gen:custom-write
  [(define write-proc datetime-write-proc)]

  #:property prop:serializable
  (make-serialize-info (λ (dt)
                         (vector (Datetime-date dt)
                                 (Datetime-time dt)))
                       #'deserialize-info:datetime
                       #f
                       (or (current-load-relative-directory)
                           (current-directory))))

(define deserialize-info:datetime
  (make-deserialize-info
   Datetime
   (λ () (error "datetime cannot have cycles"))))

(module+ deserialize-info
  (provide deserialize-info:datetime))

;;;;
;; Smart constructor
(define make-datetime
  (let ()
    (define (datetime year [month 1] [day 1] [hours 0] [minutes 0] [seconds 0] [nanos 0])
      (Datetime (date year month day)
                (time hours minutes seconds nanos)))
    datetime))

(define-module-boundary-contract
  datetime
  make-datetime
  (->i ([year exact-integer?])
       ([month (integer-in 1 12)]
        [day (year month) (day-of-month/c year month)]
        [hours (integer-in 0 23)]
        [minutes (integer-in 0 59)]
        [seconds (integer-in 0 59)]
        [nanoseconds (integer-in 0 (sub1 NS/SECOND))])
       [dt datetime?]))

(define-match-expander $datetime
  (syntax-rules ()
    [(_ y mo d h mi s n) (Datetime (date y mo d) (time h mi s n))]
    [(_ d t)             (Datetime d t)])
  (make-variable-like-transformer #'datetime))

;;;;
;; Other constructors
(define (date->datetime d [t MIDNIGHT])
  (Datetime d t))

;;;;
;; Comparison
(define (datetime-comparator x y)
  (match-define (Datetime d1 t1) x)
  (match-define (Datetime d2 t2) y)
  
  (match (date-order d1 d2)
    ['= (time-order t1 t2)]
    [result result]))

(match-define (comparison datetime=? datetime<? datetime<=? datetime>? datetime>=? datetime-order)
  (build-comparison 'datetime-order datetime? datetime-comparator))

;;;;
;; Exports
(provide/contract
 [datetime?           (-> any/c boolean?)]

 [date->datetime      (->* (date?) (time?) datetime?)]

 [datetime->iso8601   (-> datetime? string?)]

 [datetime=?          (-> datetime? datetime? boolean?)]
 [datetime<?          (-> datetime? datetime? boolean?)]
 [datetime>?          (-> datetime? datetime? boolean?)]
 [datetime<=?         (-> datetime? datetime? boolean?)]
 [datetime>=?         (-> datetime? datetime? boolean?)]
 [datetime-order      order?]

 [datetime->date      (-> datetime? date?)]
 [datetime->time      (-> datetime? time?)]

 [posix->jd           (-> rational? rational?)]
 [jd->posix           (-> rational? rational?)]
 
 [datetime->jd        (-> datetime? rational?)]
 [jd->datetime        (-> real? datetime?)]

 [datetime-add-months      (-> datetime? exact-integer? datetime?)]
 [datetime-add-days        (-> datetime? exact-integer? datetime?)]
 [datetime-add-nanoseconds (-> datetime? exact-integer? datetime?)]

 [datetime-months-between      (-> datetime? datetime? exact-integer?)]
 [datetime-days-between        (-> datetime? datetime? exact-integer?)]
 [datetime-nanoseconds-between (-> datetime? datetime? exact-integer?)]

 [datetime->posix     (-> datetime? rational?)]
 [posix->datetime     (-> real? datetime?)])

(provide (rename-out [$datetime datetime]))
