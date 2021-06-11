#lang racket/base

(require racket/contract/base
         racket/generic
         "date-arithmetic-provider.rkt"
         "time-arithmetic-provider.rkt"
         "../base-date.rkt"
         "../datetime.rkt"
         "../datetime/offset.rkt"
         "../datetime/tz.rkt"
         "../offset-resolver.rkt"
         "../period.rkt")

(define-generics datetime-arithmetic-provider
  (+period datetime-arithmetic-provider p #:offset-resolver [offset-resolver])
  (-period datetime-arithmetic-provider p #:offset-resolver [offset-resolver])

  #:defaults
  ([datetime?]
   [datetime/offset?]
   [datetime/tz?]
   [period?]
   [base-date/compat?])
  
  #:fallbacks
  [(define/generic plus-period +period)
   
   (define (+period x y #:offset-resolver [offset-resolver (current-date-arithmetic-offset-resolver)])
     (define t0 (+date-period x (period->date-period y) #:offset-resolver offset-resolver))
     (+time-period t0 (period->time-period y)))

   (define (-period x y #:offset-resolver [offset-resolver (current-date-arithmetic-offset-resolver)])
     (plus-period x (period-negate y) #:offset-resolver offset-resolver))])

(provide gen:datetime-arithmetic-provider
         datetime-arithmetic-provider?)

(define arith/c
  (->i ([t datetime-arithmetic-provider?]
        [p period?])
       (#:offset-resolver [resolver offset-resolver?])
       [result datetime-arithmetic-provider?]))

(provide/contract
 [+period arith/c]
 [-period arith/c])
