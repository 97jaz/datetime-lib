#lang racket/base

(require "datetime.rkt"
         tzinfo)

(provide (all-defined-out))

(struct exn:datetime exn:fail ())
(struct exn:datetime:invalid-offset exn:datetime ())
(struct exn:datetime:invalid-pattern exn:datetime ())
(struct exn:datetime:parse exn:datetime ())

(define (raise-invalid-offset g/o target-dt target-tzid)
  (raise
   (exn:datetime:invalid-offset
    (format "Illegal moment: local time ~a ~a in time zone ~a"
            (datetime->iso8601 target-dt)
            (if (tzgap? g/o)
                "does not exist"
                "is ambiguous")
            target-tzid)
    (current-continuation-marks))))