#lang racket/base

(require racket/contract/base
         racket/generic
         racket/match
         "../base-date.rkt"
         "../date.rkt"
         "../offset-resolver.rkt"
         "../period.rkt"
         "../datetime.rkt"
         "../datetime/offset.rkt"
         "../datetime/tz.rkt")

(define-syntax-rule (define/resolver name fn)
  (define (name t n #:offset-resolver [resolver (current-date-arithmetic-offset-resolver)])
    (fn t n)))

(define-generics date-arithmetic-provider
  (+years    date-arithmetic-provider n #:offset-resolver [offset-resolver])
  (+months   date-arithmetic-provider n #:offset-resolver [offset-resolver])
  (+weeks    date-arithmetic-provider n #:offset-resolver [offset-resolver])
  (+days     date-arithmetic-provider n #:offset-resolver [offset-resolver])

  (-years    date-arithmetic-provider n #:offset-resolver [offset-resolver])
  (-months   date-arithmetic-provider n #:offset-resolver [offset-resolver])
  (-weeks    date-arithmetic-provider n #:offset-resolver [offset-resolver])
  (-days     date-arithmetic-provider n #:offset-resolver [offset-resolver])

  (+date-period date-arithmetic-provider p #:offset-resolver [offset-resolver])
  (-date-period date-arithmetic-provider p #:offset-resolver [offset-resolver])

  #:defaults
  ([date?
    (define/resolver +years       date-add-years)
    (define/resolver +months      date-add-months)
    (define/resolver +days        date-add-days)]

   [datetime?
    (define/resolver +months      datetime-add-months)
    (define/resolver +days        datetime-add-days)]

   [datetime/offset?
    (define/resolver +months      datetime/offset-add-months)
    (define/resolver +days        datetime/offset-add-days)]

   [datetime/tz?
    (define +months               datetime/tz-add-months)
    (define +days                 datetime/tz-add-days)]

   [period?
    (define/resolver +years       period-add-years)
    (define/resolver +months      period-add-months)
    (define/resolver +weeks       period-add-weeks)
    (define/resolver +days        period-add-days)
    (define/resolver +date-period period-add-date-period)]

   [base-date/compat?
    (define/resolver +months      base-date-add-months)
    (define/resolver +days        base-date-add-days)])

  #:fallbacks
  [(define/generic plus-years +years)
   (define/generic plus-months +months)
   (define/generic plus-weeks +weeks)
   (define/generic plus-days +days)
   (define/generic plus-date-period +date-period)
   
   (define (+years t n #:offset-resolver [offset-resolver (current-date-arithmetic-offset-resolver)])
     (plus-months t (* 12 n) #:offset-resolver offset-resolver))

   (define (+weeks t n #:offset-resolver [offset-resolver (current-date-arithmetic-offset-resolver)])
     (plus-days t (* 7 n) #:offset-resolver offset-resolver))

   (define (-years t n #:offset-resolver [offset-resolver (current-date-arithmetic-offset-resolver)])
     (plus-years t (- n) #:offset-resolver offset-resolver))

   (define (-months t n #:offset-resolver [offset-resolver (current-date-arithmetic-offset-resolver)])
     (plus-months t (- n) #:offset-resolver offset-resolver))

   (define (-weeks t n #:offset-resolver [offset-resolver (current-date-arithmetic-offset-resolver)])
     (plus-weeks t (- n) #:offset-resolver offset-resolver))

   (define (-days t n #:offset-resolver [offset-resolver (current-date-arithmetic-offset-resolver)])
     (plus-days t (- n) #:offset-resolver offset-resolver))

   (define (+date-period t p #:offset-resolver [offset-resolver (current-date-arithmetic-offset-resolver)])
     (match-define (period [years y] [months m] [weeks w] [days d]) p)
     (plus-days (plus-months t (+ (* 12 y) m) #:offset-resolver offset-resolver)
                (+ (* 7 w) d) #:offset-resolver offset-resolver))

   (define (-date-period t p #:offset-resolver [offset-resolver (current-date-arithmetic-offset-resolver)])
     (plus-date-period t (period-negate p) #:offset-resolver offset-resolver))])

(provide gen:date-arithmetic-provider
         date-arithmetic-provider?)

(define arith/c (->i ([t date-arithmetic-provider?]
                      [n exact-integer?])
                     (#:offset-resolver [resolver offset-resolver?])
                     [result date-arithmetic-provider?]))

(define period-arith/c (->i ([t date-arithmetic-provider?]
                             [p date-period?])
                            (#:offset-resolver [resolver offset-resolver?])
                            [result date-arithmetic-provider?]))

(provide/contract
 [+years  arith/c]
 [-years  arith/c]
 [+months arith/c]
 [-months arith/c]
 [+weeks  arith/c]
 [-weeks  arith/c]
 [+days   arith/c]
 [-days   arith/c]

 [+date-period period-arith/c]
 [-date-period period-arith/c])
