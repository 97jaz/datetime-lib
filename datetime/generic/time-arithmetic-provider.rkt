#lang racket/base

(require racket/generic
         "../base-date.rkt"
         "../datetime.rkt"
         "../datetime/offset.rkt"
         "../datetime/tz.rkt"
         "../period.rkt"
         "../private/time/util.rkt"
         "../time.rkt"
         (prefix-in base: (only-in racket/base date?))
         (for-syntax racket/base
                     racket/syntax))

(provide (all-defined-out))

(define-generics time-arithmetic-provider
  (+hours        time-arithmetic-provider n)
  (+minutes      time-arithmetic-provider n)
  (+seconds      time-arithmetic-provider n)
  (+milliseconds time-arithmetic-provider n)
  (+microseconds time-arithmetic-provider n)
  (+nanoseconds  time-arithmetic-provider n)

  (-hours        time-arithmetic-provider n)
  (-minutes      time-arithmetic-provider n)
  (-seconds      time-arithmetic-provider n)
  (-milliseconds time-arithmetic-provider n)
  (-microseconds time-arithmetic-provider n)
  (-nanoseconds  time-arithmetic-provider n)

  (+time-period  time-arithmetic-provider p)
  (-time-period  time-arithmetic-provider p)

  #:defaults
  ([time?
    (define +nanoseconds time-add-nanoseconds)]

   [datetime?
    (define +nanoseconds datetime-add-nanoseconds)]

   [datetime/offset?
    (define +nanoseconds datetime/offset-add-nanoseconds)]

   [datetime/tz?
    (define +nanoseconds datetime/tz-add-nanoseconds)]

   [period?
    (define +hours        period-add-hours)
    (define +minutes      period-add-minutes)
    (define +seconds      period-add-seconds)
    (define +milliseconds period-add-milliseconds)
    (define +microseconds period-add-microseconds)
    (define +nanoseconds  period-add-nanoseconds)
    (define +time-period  period-add-time-period)]

   [base:date?
    (define +nanoseconds  base-date-add-nanoseconds)])

  #:fallbacks
  [(define/generic plus-ns +nanoseconds)
   (define/generic plus-time-period +time-period)

   (define-syntax (def+ stx)
     (syntax-case stx ()
       [(_ unit factor)
        (with-syntax ([+name (format-id #'unit "+~a" #'unit)]
                      [-name (format-id #'unit "-~a" #'unit)]
                      [+generic (format-id #'unit "+~a/generic" #'unit)])
          #'(begin
              (define/generic +generic +name)
              
              (define (+name t n)
                (plus-ns t (* n factor)))

              (define (-name t n)
                (+generic t (- n)))))]))

   (def+ hours NS/HOUR)
   (def+ minutes NS/MINUTE)
   (def+ seconds NS/SECOND)
   (def+ milliseconds NS/MILLI)
   (def+ microseconds NS/MICRO)

   (define (-nanoseconds t n) (plus-ns t (- n)))

   (define (+time-period t p) (plus-ns t (time-period->nanoseconds p)))
   (define (-time-period t p) (plus-time-period t (period-negate p)))])
