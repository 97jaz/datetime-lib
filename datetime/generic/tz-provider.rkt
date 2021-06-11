#lang racket/base

(require racket/contract/base
         racket/generic
         "../datetime/tz.rkt"
         "../zone.rkt")

(define-generics tz-provider
  (->tz          tz-provider)
  (->datetime/tz tz-provider [tz])

  #:defaults
  ([datetime/tz?
    (define ->tz          datetime/tz->tz)
    (define ->datetime/tz datetime/tz-at-tz)]))

(provide gen:tz-provider
         tz-provider?)

(provide/contract
 [->tz           (-> tz-provider? tz/c)]
 [->datetime/tz  (->* (tz-provider?) (tz/c) datetime/tz?)])
