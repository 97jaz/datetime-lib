#lang racket/base

(require racket/contract/base
         racket/match
         tzinfo
         "datetime.rkt"
         "exn.rkt"
         "private/time/util.rkt"
         "zone.rkt")

(struct gap-resolver (resolve))
(struct overlap-resolver (resolve))
(struct offset-resolver (gap overlap))

(define (resolve-gap resolver gap dt tz)
  (match-define (offset-resolver (gap-resolver f) _) resolver)
  (f gap dt tz))

(define (resolve-overlap resolver gap dt poff tz)
  (match-define (offset-resolver _ (overlap-resolver f)) resolver)
  (f gap dt poff tz))

(define gap-resolver/raise
  (gap-resolver
   (λ (gap dt tz)
     (raise-invalid-offset gap dt tz))))

(define gap-resolver/pre
  (gap-resolver
   (λ (gap dt tz)
     (match-define (tzgap tm _ _) gap)
     (- tm (/ 1 NS/SECOND)))))

(define gap-resolver/post
  (gap-resolver
   (λ (gap dt tz)
     (match-define (tzgap tm (tzoffset delta1 _ _) (tzoffset delta2 _ _)) gap)
     (+ tm (- delta2 delta1)))))

(define gap-resolver/push
  (gap-resolver
   (λ (gap dt tz)
     (match-define (tzgap tm (tzoffset delta1 _ _) (tzoffset delta2 _ _)) gap)
     (+ (datetime->posix dt) (- delta2 delta1) (- delta2)))))

(define overlap-resolver/raise
  (overlap-resolver
   (λ (overlap dt poff tz)
     (raise-invalid-offset overlap dt tz))))

(define overlap-resolver/pre
  (overlap-resolver
   (λ (overlap dt poff tz)
     (match-define (tzoverlap (tzoffset delta _ _) _) overlap)
     (- (datetime->posix dt) delta))))

(define overlap-resolver/post
  (overlap-resolver
   (λ (overlap dt poff tz)
     (match-define (tzoverlap _ (tzoffset delta _ _)) overlap)
     (- (datetime->posix dt) delta))))

(define overlap-resolver/retain
  (overlap-resolver
   (λ (overlap dt poff tz)
     (match-define (tzoverlap (tzoffset d1 _ _) (tzoffset d2 _ _)) overlap)
     (define offset (if (and poff (= d2 poff)) d2 d1))
     (- (datetime->posix dt) offset))))
  

(define resolve/raise (offset-resolver gap-resolver/raise overlap-resolver/raise))
(define resolve/retain (offset-resolver gap-resolver/push overlap-resolver/retain))

(define current-offset-resolver (make-parameter resolve/raise))
(define current-date-arithmetic-offset-resolver (make-parameter resolve/retain))

(define (with-gap-resolver offr gapr)
  (offset-resolver gapr (offset-resolver-overlap offr)))

(define (with-overlap-resolver offr overr)
  (offset-resolver (offset-resolver-gap offr) overr))

(provide/contract
 [gap-resolver?           (-> any/c boolean?)]
 [overlap-resolver?       (-> any/c boolean?)]
 [offset-resolver?        (-> any/c boolean?)]

 [offset-resolver         (-> gap-resolver? overlap-resolver? offset-resolver?)]
 [with-gap-resolver       (-> offset-resolver? gap-resolver? offset-resolver?)]
 [with-overlap-resolver   (-> offset-resolver? overlap-resolver? offset-resolver?)]

 [resolve-gap             (-> offset-resolver? tzgap? datetime? tz/c rational?)]
 [resolve-overlap         (-> offset-resolver? tzoverlap? datetime? (or/c utc-offset/c #f) tz/c rational?)]

 [gap-resolver/raise      gap-resolver?]
 [gap-resolver/pre        gap-resolver?]
 [gap-resolver/post       gap-resolver?]
 [gap-resolver/push       gap-resolver?]

 [overlap-resolver/raise  overlap-resolver?]
 [overlap-resolver/pre    overlap-resolver?]
 [overlap-resolver/post   overlap-resolver?]
 [overlap-resolver/retain overlap-resolver?]

 [resolve/raise                offset-resolver?]
 [resolve/retain               offset-resolver?]

 [current-offset-resolver                 (parameter/c offset-resolver?)]
 [current-date-arithmetic-offset-resolver (parameter/c offset-resolver?)])
