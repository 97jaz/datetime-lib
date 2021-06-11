#lang racket/base

(require racket/contract/base
         racket/generic
         racket/match
         racket/function
         syntax/parse/define
         "date-arithmetic-provider.rkt"
         "../base-date.rkt"
         "../date.rkt"
         "../datetime.rkt"
         "../datetime/offset.rkt"
         "../datetime/tz.rkt"
         "../period.rkt"
         (for-syntax racket/base
                     racket/sequence
                     racket/syntax))

(define-generics date-provider
  (same-date-provider-type? date-provider other)
  
  (->date       date-provider)
  (->jdn        date-provider)
  (->year       date-provider)
  (->quarter    date-provider)
  (->month      date-provider)
  (->day        date-provider)
  (->wday       date-provider)
  (->yday       date-provider)
  (->iso-week   date-provider)
  (->iso-wyear  date-provider)
  (->iso-wday   date-provider)

  (sunday?      date-provider)
  (monday?      date-provider)
  (tuesday?     date-provider)
  (wednesday?   date-provider)
  (thursday?    date-provider)
  (friday?      date-provider)
  (saturday?    date-provider)

  (years-between   date-provider other)
  (months-between  date-provider other)
  (weeks-between   date-provider other)
  (days-between    date-provider other)

  (date-period-between date-provider other [fields])

  #:defaults
  ([date?
    (define (same-date-provider-type? x y) (date? y))
    
    (define ->date         identity)
    (define ->year         date->year)
    (define ->month        date->month)
    (define ->day          date->day)
    (define ->jdn          date->jdn)
    (define ->quarter      date->quarter)
    (define ->wday         date->wday)
    (define ->yday         date->yday)
    (define ->iso-week     date->iso-week)
    (define ->iso-wyear    date->iso-wyear)
    (define ->iso-wday     date->iso-wday)

    (define months-between date-months-between)
    (define days-between   date-days-between)]

   [datetime?
    (define (same-date-provider-type? x y) (datetime? y))
    
    (define ->date         datetime->date)
    (define months-between datetime-months-between)
    (define days-between   datetime-days-between)]

   [datetime/offset?
    (define (same-date-provider-type? x y) (datetime/offset? y))
    
    (define ->date         datetime/offset->date)
    (define months-between datetime/offset-months-between)
    (define days-between   datetime/offset-days-between)]

   [datetime/tz?
    (define (same-date-provider-type? x y) (datetime/tz? y))
    
    (define ->date         datetime/tz->date)
    (define months-between datetime/tz-months-between)
    (define days-between   datetime/tz-days-between)]

   [base-date/compat?
    (define (same-date-provider-type? x y) (base-date/compat? y))
    
    (define ->date         base-date->date)
    (define months-between base-date-months-between)
    (define days-between   base-date-days-between)])

  #:fallbacks
  [(define/generic as-date ->date)
   (define/generic dow ->wday)
   (define/generic y-between years-between)
   (define/generic m-between months-between)
   (define/generic w-between weeks-between)
   (define/generic d-between days-between)

   (define-syntax-parser define-date-fallbacks
     [(_ name:id ...)
      (with-syntax ([(tmp ...) (generate-temporaries #'(name ...))])
        #'(begin
            (begin
              (define/generic tmp name)
              (define name (compose1 tmp as-date)))
            ...))])

   (define-date-fallbacks
     ->jdn
     ->year
     ->quarter
     ->month
     ->day
     ->wday
     ->yday
     ->iso-week
     ->iso-wyear
     ->iso-wday)
   
   (define-syntax-parser define-dow-predicates
     [(_ name:id ...)
      (with-syntax ([(idx ...) (sequence->list (in-range (length (syntax->list #'(name ...)))))])
        #'(begin
            (define (name x) (= (dow x) idx))
            ...))])

   (define-dow-predicates
     sunday?
     monday?
     tuesday?
     wednesday?
     thursday?
     friday?
     saturday?)

   (define (years-between x y) (quotient (m-between x y) 12))
   (define (weeks-between x y) (quotient (d-between x y) 7))

   (define (date-period-between x y [fields date-units])
     (define-values (result _)
       (for/fold ([p empty-period] [t x]) ([f (in-list date-units)])
         (cond [(memq f fields)
                (define-values (between add)
                  (match f
                    ['years (values y-between +years)]
                    ['months (values m-between +months)]
                    ['weeks (values w-between +weeks)]
                    ['days (values d-between +days)]))

                (define n (between t y))
                (values (period-set p f n) (add t n))]
               [else
                (values p t)])))
     result)])


(provide gen:date-provider
         date-provider?)

(define difference/c (->i ([x date-provider?]
                           [y (x) (λ (y) (same-date-provider-type? x y))])
                          [result exact-integer?]))

(provide/contract
 [same-date-provider-type? (-> date-provider? date-provider? boolean?)]
 [->date                   (-> date-provider? date?)]
 [->jdn                    (-> date-provider? exact-integer?)]
 [->year                   (-> date-provider? exact-integer?)]
 [->quarter                (-> date-provider? (integer-in 1 4))]
 [->month                  (-> date-provider? (integer-in 1 12))]
 [->day                    (-> date-provider? (integer-in 1 31))]
 [->wday                   (-> date-provider? (integer-in 0 6))]
 [->yday                   (-> date-provider? (integer-in 1 366))]
 [->iso-week               (-> date-provider? (integer-in 1 53))]
 [->iso-wyear              (-> date-provider? exact-integer?)]
 [->iso-wday               (-> date-provider? (integer-in 1 7))]
 [sunday?                  (-> date-provider? boolean?)]
 [monday?                  (-> date-provider? boolean?)]
 [tuesday?                 (-> date-provider? boolean?)]
 [wednesday?               (-> date-provider? boolean?)]
 [thursday?                (-> date-provider? boolean?)]
 [friday?                  (-> date-provider? boolean?)]
 [saturday?                (-> date-provider? boolean?)]

 [years-between            difference/c]
 [months-between           difference/c]
 [weeks-between            difference/c]
 [days-between             difference/c]

 [date-period-between      (->i ([x date-provider?]
                                 [y (x) (λ (y) (same-date-provider-type? x y))])
                                ([xs (listof date-unit/c)])
                                [result date-period?])])

