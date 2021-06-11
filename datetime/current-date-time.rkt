#lang racket/base

(require racket/contract/base
         "clock.rkt"
         "date.rkt"
         "datetime.rkt"
         "datetime/tz.rkt"
         "datetime/offset.rkt"
         "generic/date-provider.rkt"
         "generic/datetime-provider.rkt"
         "generic/time-provider.rkt"
         "time.rkt"
         "zone.rkt")

(provide/contract
 [current-datetime/tz         (->* () (tz/c) datetime/tz?)]
 [current-datetime/offset     (->* () ((or/c tz/c utc-offset/c)) datetime/offset?)]
 [current-datetime            (->* () ((or/c tz/c utc-offset/c)) datetime?)]
 [current-date                (->* () ((or/c tz/c utc-offset/c)) date?)]
 [current-time                (->* () ((or/c tz/c utc-offset/c)) time?)])

(define (current-datetime/tz [tz (current-time-zone)])
  (posix->datetime/tz ((current-clock)) #:tz tz))

(define (current-datetime/offset [tz-or-off (current-time-zone)])
  (if (tz/c tz-or-off)
      (datetime/tz->datetime/offset (current-datetime/tz tz-or-off))
      (posix->datetime/offset ((current-clock)) #:offset tz-or-off)))

(define (current-datetime [tz-or-off (current-time-zone)])
  (->datetime (current-datetime/offset tz-or-off)))

(define (current-date [tz-or-off (current-time-zone)])
  (->date (current-datetime/offset tz-or-off)))

(define (current-time [tz-or-off (current-time-zone)])
  (->time (current-datetime/offset tz-or-off)))