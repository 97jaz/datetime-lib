#lang racket/base

(require racket/contract/base
         racket/match
         (prefix-in b: (only-in racket/base
                                date
                                date?
                                date-year
                                date-month
                                date-day
                                date-hour
                                date-minute
                                date-second
                                date-dst?
                                date-time-zone-offset
                                date*-time-zone-name
                                date*
                                date*?
                                date*-nanosecond))
         "date.rkt"
         "datetime.rkt"
         "datetime/offset.rkt"
         "period.rkt"
         "private/date/contract.rkt"
         "time.rkt"
         "zone.rkt")

(define (base-date/compat? d)
  (and (b:date? d)
       ;; not a (pseudo-)leap second
       (< (date-second d) 60)
       ;; at a legal UTC offset
       (utc-offset/c (date-time-zone-offset d))
       ;; on an actual day
       ((day-of-month/c (date-year d) (date-month d)) (date-day d))))

(define (base-date->date d)
  (date (b:date-year d)
        (b:date-month d)
        (b:date-day d)))

(define (base-date->time d)
  (time (b:date-hour d)
        (b:date-minute d)
        (b:date-second d)
        (if (b:date*? d)
            (b:date*-nanosecond d)
            0)))

(define (base-date->datetime d)
  (date->datetime (base-date->date d) (base-date->time d)))

(define (base-date->utc-datetime d)
  (datetime/offset->utc-datetime
   (base-date->datetime/offset d)))

(define (base-date->posix d)
  (datetime/offset->posix (base-date->datetime/offset d)))

(define (base-date->jd d)
  (datetime/offset->jd (base-date->datetime/offset d)))

(define (base-date->datetime/offset d)
  (datetime/offset (b:date-year d)
                   (b:date-month d)
                   (b:date-day d)
                   (b:date-hour d)
                   (b:date-minute d)
                   (b:date-second d)
                   (if (b:date*? d)
                       (b:date*-nanosecond d)
                       0)
                   #:offset (b:date-time-zone-offset d)))

(define (base-date->datetime/offset* d [off (b:date-time-zone-offset d)])
  (datetime/offset-at-utc-offset
   (base-date->datetime/offset d)
   off))

(define base-date->utc-offset b:date-time-zone-offset)

(define (datetime/offset->base-date x orig)
  (match-define (datetime/offset y mo d h mi s n off) x)
  (define dt (date y mo d))
  (if (b:date*? orig)
      (b:date* s mi h d mo y
               (date->wday dt)
               (sub1 (date->yday dt)) ;; date* indexes year-day from 0
               (date-dst? orig) off n (date*-time-zone-name orig))
      (b:date s mi h d mo y
              (date->wday dt)
              (sub1 (date->yday dt))
              (date-dst? orig)
              off)))

(define-syntax-rule (define-between name fn)
  (define (name x y)
    (fn (base-date->datetime/offset x)
        (base-date->datetime/offset y))))

(define-between base-date-months-between      datetime/offset-months-between)
(define-between base-date-days-between        datetime/offset-days-between)
(define-between base-date-nanoseconds-between datetime/offset-nanoseconds-between)

(define-syntax-rule (define-date-arith name fn)
  (define (name d n)
    (datetime/offset->base-date
     (fn (base-date->datetime/offset d) n)
     d)))

(define-date-arith base-date-add-months      datetime/offset-add-months)
(define-date-arith base-date-add-days        datetime/offset-add-days)
(define-date-arith base-date-add-nanoseconds datetime/offset-add-nanoseconds)

(provide/contract
 [base-date/compat?     (-> any/c boolean?)]
 
 [base-date->date               (-> b:date? date?)]
 [base-date->time               (-> b:date? time?)]
 [base-date->datetime           (-> b:date? datetime?)]
 [base-date->datetime/offset*   (->* (b:date?) (utc-offset/c) datetime/offset?)]
 [base-date->utc-offset         (-> b:date? utc-offset/c)]
 [base-date->utc-datetime       (-> b:date? datetime?)]

 [base-date->posix              (-> b:date? rational?)]
 [base-date->jd                 (-> b:date? rational?)]
 
 [base-date-months-between      (-> b:date? b:date? exact-integer?)]
 [base-date-days-between        (-> b:date? b:date? exact-integer?)]
 [base-date-nanoseconds-between (-> b:date? b:date? exact-integer?)]

 [base-date-add-months         (-> b:date? exact-integer? b:date?)]
 [base-date-add-days           (-> b:date? exact-integer? b:date?)]
 [base-date-add-nanoseconds    (-> b:date? exact-integer? b:date?)])
