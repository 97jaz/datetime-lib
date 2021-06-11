#lang racket/base

(require racket/contract/base
         racket/match
         tzinfo)

;;;;
;; Contracts
(define (tz/c tzid)
  (and (string? tzid)
       (tzid-exists? tzid)))

(define utc-offset/c (integer-in -64800 64800))

;;;;
;; Parameters
(define current-time-zone
  (make-parameter (system-tzid)))


;;;;
;; Utilities
(define (hours->utc-offset h)
  (* h 60 60))

(provide/contract
 [tz/c               flat-contract?]
 [utc-offset/c       flat-contract?]
 
 [current-time-zone  (parameter/c tz/c)]

 [hours->utc-offset  (-> (integer-in -18 18) utc-offset/c)])
