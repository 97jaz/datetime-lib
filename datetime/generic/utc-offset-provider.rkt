#lang racket/base

(require racket/contract/base
         racket/function
         racket/generic
         "../base-date.rkt"
         "../datetime.rkt"
         "../datetime/offset.rkt"
         "../datetime/tz.rkt"
         "../zone.rkt")

(define-generics utc-offset-provider
  (->datetime/offset utc-offset-provider [offset])
  (->utc-offset      utc-offset-provider)
  (->local-datetime  utc-offset-provider)
  (->utc-datetime    utc-offset-provider)

  #:defaults
  ([datetime/offset?
    (define ->datetime/offset datetime/offset-at-utc-offset)
    (define ->utc-offset      datetime/offset->utc-offset)
    (define ->local-datetime  datetime/offset->local-datetime)
    (define ->utc-datetime    datetime/offset->utc-datetime)]

   [datetime/tz?
    (define ->datetime/offset datetime/tz->datetime/offset)
    (define ->utc-offset      datetime/tz->utc-offset)
    (define ->local-datetime  datetime/tz->local-datetime)
    (define ->utc-datetime    datetime/tz->utc-datetime)]

   [base-date/compat?
    (define ->datetime/offset base-date->datetime/offset*)
    (define ->utc-offset      base-date->utc-offset)
    (define ->local-datetime  base-date->datetime)
    (define ->utc-datetime    base-date->utc-datetime)]))

(provide gen:utc-offset-provider
          utc-offset-provider?)

(provide/contract
 [->datetime/offset (->* (utc-offset-provider?) (utc-offset/c) datetime/offset?)]
 [->utc-offset      (-> utc-offset-provider? utc-offset/c)]
 [->local-datetime  (-> utc-offset-provider? datetime?)]
 [->utc-datetime    (-> utc-offset-provider? datetime?)])
