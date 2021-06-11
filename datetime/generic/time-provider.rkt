#lang racket/base

(require racket/contract/base
         racket/function
         racket/generic
         racket/match
         syntax/parse/define
         "time-arithmetic-provider.rkt"
         "../base-date.rkt"
         "../datetime.rkt"
         "../datetime/offset.rkt"
         "../datetime/tz.rkt"
         "../period.rkt"
         "../time.rkt"
         "../private/time/util.rkt"
         (for-syntax racket/base))

(define-generics time-provider
  (same-time-provider-type? time-provider other)
  
  (->time         time-provider)
  (->hours        time-provider)
  (->minutes      time-provider)
  (->seconds      time-provider [fractional?])
  (->milliseconds time-provider)
  (->microseconds time-provider)
  (->nanoseconds  time-provider)

  (hours-between         time-provider other)
  (minutes-between       time-provider other)
  (seconds-between       time-provider other)
  (milliseconds-between  time-provider other)
  (microseconds-between  time-provider other)
  (nanoseconds-between   time-provider other)
  (time-period-between   time-provider other [fields])

  #:defaults
  ([time?
    (define (same-time-provider-type? x y) (time? y))
    
    (define ->time              identity)
    (define ->hours             time->hours)
    (define ->minutes           time->minutes)
    (define ->seconds           time->seconds)
    (define ->milliseconds      time->milliseconds)
    (define ->microseconds      time->microseconds)
    (define ->nanoseconds       time->nanoseconds)
    (define nanoseconds-between time-nanoseconds-between)]

   [datetime?
    (define (same-time-provider-type? x y) (datetime? y))
    
    (define ->time              datetime->time)
    (define nanoseconds-between datetime-nanoseconds-between)]

   [datetime/offset?
    (define (same-time-provider-type? x y) (datetime/offset? y))
    
    (define ->time              datetime/offset->time)
    (define nanoseconds-between datetime/offset-nanoseconds-between)]

   [datetime/tz?
    (define (same-time-provider-type? x y) (datetime/tz? y))
    
    (define ->time              datetime/tz->time)
    (define nanoseconds-between datetime/tz-nanoseconds-between)]

   [base-date/compat?
    (define (same-time-provider-type? x y) (base-date/compat? y))
    
    (define ->time              base-date->time)
    (define nanoseconds-between base-date-nanoseconds-between)])

  #:fallbacks
  [(define/generic to-time ->time)
   (define/generic to-secs ->seconds)
   (define/generic hrs-between hours-between)
   (define/generic min-between minutes-between)
   (define/generic sec-between seconds-between)
   (define/generic mil-between milliseconds-between)
   (define/generic mic-between microseconds-between)
   (define/generic nan-between nanoseconds-between)

   (define-syntax-parser define-time-fallbacks
     [(_ name:id ...)
      (with-syntax ([(tmp ...) (generate-temporaries #'(name ...))])
        #'(begin
            (begin
              (define/generic tmp name)
              (define name (compose1 tmp to-time)))
            ...))])

   (define-time-fallbacks
     ->hours
     ->minutes
     ->milliseconds
     ->microseconds
     ->nanoseconds)

   (define (->seconds t [fractional? #f])
     (to-secs (to-time t) fractional?))

   (define-syntax-rule (define-between name divisor)
     (define (name x y)
       (quotient (nan-between x y) divisor)))

   (define-between hours-between NS/HOUR)
   (define-between minutes-between NS/MINUTE)
   (define-between seconds-between NS/SECOND)
   (define-between milliseconds-between NS/MILLI)
   (define-between microseconds-between NS/MICRO)

   (define (time-period-between x y [fields time-units])
     (define-values (result _)
       (for/fold ([p empty-period] [t x]) ([f (in-list time-units)])
         (cond [(memq f fields)
                (define-values (between add)
                  (match f
                    ['hours (values hrs-between +hours)]
                    ['minutes (values min-between +minutes)]
                    ['seconds (values sec-between +seconds)]
                    ['milliseconds (values mil-between +milliseconds)]
                    ['microseconds (values mic-between +microseconds)]
                    ['nanoseconds (values nan-between +nanoseconds)]))

                (define n (between t y))
                (values (period-set p f n) (add t n))]
               [else
                (values p t)])))
     result)])

(provide gen:time-provider
         time-provider?)

(define diff/c
  (->i ([x time-provider?]
        [y (x) (λ (y) (same-time-provider-type? x y))])
       [result exact-integer?]))

(provide/contract
 [same-time-provider-type? (-> time-provider? time-provider? boolean?)]

 [->time                   (-> time-provider? time?)]
 [->hours                  (-> time-provider? (integer-in 0 23))]
 [->minutes                (-> time-provider? (integer-in 0 59))]
 [->seconds                (->* (time-provider?) (boolean?) (and/c rational? (>=/c 0) (</c 60)))]
 [->milliseconds           (-> time-provider? (integer-in 0 999))]
 [->microseconds           (-> time-provider? (integer-in 0 999999))]
 [->nanoseconds            (-> time-provider? (integer-in 9 999999999))]

 [hours-between            diff/c]
 [minutes-between          diff/c]
 [seconds-between          diff/c]
 [milliseconds-between     diff/c]
 [microseconds-between     diff/c]
 [nanoseconds-between      diff/c]
 
 [time-period-between      (->i ([x time-provider?]
                                 [y (x) (λ (y) (same-time-provider-type? x y))])
                                ([units (listof time-unit/c)])
                                [result time-period?])])
