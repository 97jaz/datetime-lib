#lang racket/base

(require racket/contract/base
         racket/function
         racket/generic
         racket/list
         "date-provider.rkt"
         "time-provider.rkt"
         "date-arithmetic-provider.rkt"
         "datetime-arithmetic-provider.rkt"
         "../base-date.rkt"
         "../datetime.rkt"
         "../datetime/offset.rkt"
         "../datetime/tz.rkt"
         "../period.rkt")

(define-generics datetime-provider
  (same-datetime-provider-type? datetime-provider other)
  
  (->datetime datetime-provider)
  (->posix    datetime-provider)
  (->jd       datetime-provider)

  (period-between datetime-provider other [fields])

  #:defaults
  ([datetime?
    (define (same-datetime-provider-type? x y) (datetime? y))
    
    (define ->datetime     identity)
    (define ->posix        datetime->posix)
    (define ->jd           datetime->jd)]

   [datetime/offset?
    (define (same-datetime-provider-type? x y) (datetime/offset? y))
    
    (define ->datetime     datetime/offset->local-datetime)
    (define ->posix        datetime/offset->posix)
    (define ->jd           datetime/offset->jd)]

   [datetime/tz?
    (define (same-datetime-provider-type? x y) (datetime/tz? y))
    
    (define ->datetime     datetime/tz->local-datetime)
    (define ->posix        datetime/tz->posix)
    (define ->jd           datetime/tz->jd)]
    
   [base-date/compat?
    (define (same-datetime-provider-type? x y) (base-date/compat? y))
    
    (define ->datetime     base-date->datetime)
    (define ->posix        base-date->posix)
    (define ->jd           base-date->jd)])
   
  #:fallbacks
  [(define (period-between x y [fields temporal-units])
     (define-values (date-fields time-fields) (partition (λ (f) (memq f date-units)) fields))
     (define dp (date-period-between x y date-fields))
     (define t (+date-period x dp))
     (define tp (time-period-between t y time-fields))
     (+period dp tp))])

(provide gen:datetime-provider
         datetime-provider?)

(provide/contract
 [same-datetime-provider-type? (-> datetime-provider? datetime-provider? boolean?)]
 
 [->datetime     (-> datetime-provider? datetime?)]
 [->posix        (-> datetime-provider? rational?)]
 [->jd           (-> datetime-provider? rational?)]
 [period-between (->i ([t1 datetime-provider?]
                       [t2 (t1) (λ (t2) (same-datetime-provider-type? t1 t2))])
                      ([xs (listof temporal-unit/c)])
                      [result period?])])
