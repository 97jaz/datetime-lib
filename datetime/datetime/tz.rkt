#lang racket/base

(require racket/contract
         racket/match
         racket/serialize
         data/order
         tzinfo
         "offset.rkt"
         "../date.rkt"
         "../datetime.rkt"
         "../offset-resolver.rkt"
         "../time.rkt"
         "../zone.rkt"
         "../private/compare.rkt"
         "../private/date/contract.rkt"
         "../private/time/util.rkt"

         (for-syntax racket/base
                     syntax/transformer))

;;;;
;; gen:equal+hash
(define (datetime/tz-equal-proc x y fn)
  (match-define (Datetime/tz do1 z1) x)
  (match-define (Datetime/tz do2 z2) y)
  (and (fn do1 do2) (fn z1 z2)))

(define (datetime/tz-hash-proc x fn)
  (match-define (Datetime/tz do z) x)
  (bitwise-xor (fn do) (fn z)))

;;;;
;; gen:custom-write
(define (datetime/tz-write-proc x out mode)
  (if mode
      (fprintf out "#<datetime/tz ~a>" (datetime/tz->string x))
      (display (datetime/tz->string x) out)))

(define (datetime/tz->string x)
  (match-define (Datetime/tz do z) x)
  (format "~a[~a]"
          (datetime/offset->string do)
          z))

;;;;
;; Time zone adjustment
(define (datetime/tz-at-tz x [z (datetime/tz->tz x)])
  (match x
    [(Datetime/tz _ (== z))
     x]

    [(Datetime/tz do _)
     (match-define (tzoffset off _ _) (utc-seconds->tzoffset z (datetime/offset->posix do)))
     (Datetime/tz (datetime/offset-at-utc-offset do off) z)]))

;;;;
;; Preducate
(define (datetime/tz? x)
  (Datetime/tz? x))

;;;;
;; Accessors
(define (datetime/tz->datetime/offset t [off (datetime/tz->utc-offset t)])
  (match-define (Datetime/tz dto _) t)
  (datetime/offset-at-utc-offset dto off))

(define (datetime/tz->date t)
  (match-define (Datetime/tz dto _) t)
  (datetime/offset->date dto))

(define (datetime/tz->time t)
  (match-define (Datetime/tz dto _) t)
  (datetime/offset->time dto))

(define (datetime/tz->local-datetime t)
  (match-define (Datetime/tz dto _) t)
  (datetime/offset->local-datetime dto))

(define (datetime/tz->utc-datetime t)
  (match-define (Datetime/tz dto _) t)
  (datetime/offset->utc-datetime dto))

(define (datetime/tz->utc-offset t)
  (match-define (Datetime/tz (datetime/offset _ off) _) t)
  off)

(define (datetime/tz->tz t)
  (match-define (Datetime/tz _ tz) t)
  tz)

;;;;
;; Arithmetic
(define (datetime/tz-add-months
         t n
         #:offset-resolver [resolver (current-date-arithmetic-offset-resolver)])
  (match-define (Datetime/tz (datetime/offset dt off) tz) t)
  (datetime+original-offset->datetime/tz (datetime-add-months dt n) off tz resolver))

(define (datetime/tz-add-days
         t n
         #:offset-resolver [resolver (current-date-arithmetic-offset-resolver)])
  (match-define (Datetime/tz (datetime/offset dt off) tz) t)
  (datetime+original-offset->datetime/tz (datetime-add-days dt n) off tz resolver))

(define (datetime/tz-add-nanoseconds t n)
  (match-define (Datetime/tz dto tz) t)
  (posix->datetime/tz
   (+ (datetime/offset->posix dto)
      (/ n NS/SECOND))
   #:tz tz))

;;;;
;; Difference
(define-syntax-rule (define-between name fn)
  (define (name x y)
    (match-define (Datetime/tz dox tz) x)
    (match-define (Datetime/tz doy _) (datetime/tz-at-tz y tz))
    (fn (datetime/offset->local-datetime dox)
        (datetime/offset->local-datetime doy))))

(define-between datetime/tz-months-between      datetime-months-between)
(define-between datetime/tz-days-between        datetime-days-between)

(define (datetime/tz-nanoseconds-between x y)
  (datetime-nanoseconds-between
   (datetime/tz->utc-datetime x)
   (datetime/tz->utc-datetime y)))

;;;;
;; POSIX & Julian
(define (datetime/tz->posix t)
  (match-define (Datetime/tz dto _) t)
  (datetime/offset->posix dto))

(define (datetime/tz->jd t)
  (match-define (Datetime/tz dto _) t)
  (datetime/offset->jd dto))

(define (posix->datetime/tz seconds #:tz [tz (current-time-zone)])
  (match-define (tzoffset off _ _) (utc-seconds->tzoffset tz seconds))
  (Datetime/tz (datetime->datetime/offset
                (posix->datetime (+ seconds off)) #:offset off)
               tz))

(define (jd->datetime/tz jd #:tz [tz (current-time-zone)])
  (posix->datetime/tz (jd->posix jd) #:tz tz))

;;;;
;; Struct definition
(struct Datetime/tz (datetime/offset tz)
  #:methods gen:equal+hash
  [(define equal-proc datetime/tz-equal-proc)
   (define hash-proc  datetime/tz-hash-proc)
   (define hash2-proc datetime/tz-hash-proc)]

  #:methods gen:custom-write
  [(define write-proc datetime/tz-write-proc)]

  #:property prop:serializable
  (make-serialize-info (λ (t)
                         (match-define (Datetime/tz dto tz) t)
                         (vector dto tz))
                       #'deserialize-info:datetime/tz
                       #f
                       (or (current-load-relative-directory)
                           (current-directory))))

(define deserialize-info:datetime/tz
  (make-deserialize-info
   Datetime/tz
   (λ () (error "datetime/tz cannot have cycles"))))

(module+ deserialize-info
  (provide deserialize-info:datetime/tz))

;;;;
;; Smart constructor
(define make-datetime/tz
  (let ()
    (define (datetime/tz year [month 1] [day 1] [hours 0] [minutes 0] [seconds 0] [nanos 0]
                         #:tz [tz (current-time-zone)]
                         #:offset-resolver [resolver (current-offset-resolver)])
      (datetime->datetime/tz
       (datetime year month day hours minutes seconds nanos)
       #:tz tz
       #:offset-resolver resolver))
    datetime/tz))

(define-module-boundary-contract
  datetime/tz
  make-datetime/tz
  (->i ([year exact-integer?])
       ([month (integer-in 1 12)]
        [day (year month) (day-of-month/c year month)]
        [hours (integer-in 0 23)]
        [minutes (integer-in 0 59)]
        [seconds (integer-in 0 59)]
        [nanoseconds (integer-in 0 (sub1 NS/DAY))]
        #:tz [tz tz/c]
        #:offset-resolver [resolver offset-resolver?])
       [dt datetime/tz?]))

(define-match-expander $datetime/tz
  (syntax-rules ()
    [(_ y mo d h mi s n off tz)
     (Datetime/tz (datetime/offset (datetime (date y mo d) (time h mi s n)) off) tz)]
    [(_ d t off tz)
     (Datetime/tz (datetime/offset (datetime d t) off) tz)]
    [(_ dt off tz)
     (Datetime/tz (datetime/offset dt off) tz)]
    [(_ dto tz)
     (Datetime/tz dto tz)])
  (make-variable-like-transformer #'datetime/tz))

;;;;
;; Other constructors
(define (datetime/offset->datetime/tz dto #:tz [tz (current-time-zone)])
  (posix->datetime/tz (datetime/offset->posix dto) #:tz tz))

(define (datetime+original-offset->datetime/tz dt original-offset tz resolver)
  (match (local-seconds->tzoffset tz (datetime->posix dt))
    [(tzoffset seconds _ _)
     (Datetime/tz (datetime->datetime/offset dt #:offset seconds) tz)]
    [(? tzgap? gap)
     (posix->datetime/tz (resolve-gap resolver gap dt tz) #:tz tz)]
    [(? tzoverlap? overlap)
     (posix->datetime/tz (resolve-overlap resolver overlap dt original-offset tz) #:tz tz)]))
  
(define (datetime->datetime/tz dt
                               #:tz [tz (current-time-zone)]
                               #:offset-resolver [resolver (current-offset-resolver)])
  (datetime+original-offset->datetime/tz dt #f tz resolver))

;;;;
;; Comparison
(define (datetime/tz-comparator x y)
  (match-define (Datetime/tz dto1 _) x)
  (match-define (Datetime/tz dto2 _) y)

  (datetime/offset-order dto1 dto2))

(match-define (comparison datetime/tz=?
                          datetime/tz<?
                          datetime/tz<=?
                          datetime/tz>?
                          datetime/tz>=?
                          datetime/tz-order)
  (build-comparison 'datetime/tz-order datetime/tz? datetime/tz-comparator))


(define date-arith/c (->i ([d datetime/tz?] [n exact-integer?])
                          (#:offset-resolver [resolve offset-resolver?])
                          [r datetime/tz?]))

(provide/contract
 [datetime/tz?                          (-> any/c boolean?)]

 [datetime/tz->string                   (-> datetime/tz? string?)]

 [datetime/tz-at-tz                     (->* (datetime/tz?) (tz/c) datetime/tz?)]
 [datetime/tz->datetime/offset          (->* (datetime/tz?) (utc-offset/c) datetime/offset?)]
 [datetime/tz->date                     (-> datetime/tz? date?)]
 [datetime/tz->time                     (-> datetime/tz? time?)]
 [datetime/tz->local-datetime           (-> datetime/tz? datetime?)]
 [datetime/tz->utc-datetime             (-> datetime/tz? datetime?)]
 [datetime/tz->utc-offset               (-> datetime/tz? utc-offset/c)]
 [datetime/tz->tz                       (-> datetime/tz? tz/c)]

 [datetime/tz->posix                    (-> datetime/tz? rational?)]
 [datetime/tz->jd                       (-> datetime/tz? rational?)]

 [posix->datetime/tz                    (->i ([sec rational?]) (#:tz [tz tz/c])
                                             [r datetime/tz?])]
 [jd->datetime/tz                       (->i ([sec rational?]) (#:tz [tz tz/c])
                                             [r datetime/tz?])]
 [datetime->datetime/tz                 (->i ([dt datetime?])
                                             (#:tz [tz tz/c]
                                              #:offset-resolver [resolver offset-resolver?])
                                             [r datetime/tz?])]
 [datetime/offset->datetime/tz          (->i ([dto datetime/offset?])
                                             (#:tz [tz tz/c])
                                             [r datetime/tz?])]

 [datetime/tz-add-months                date-arith/c]
 [datetime/tz-add-days                  date-arith/c]
 [datetime/tz-add-nanoseconds           (-> datetime/tz? exact-integer? datetime/tz?)]
 
 [datetime/tz-months-between            (-> datetime/tz? datetime/tz? exact-integer?)]
 [datetime/tz-days-between              (-> datetime/tz? datetime/tz? exact-integer?)]
 [datetime/tz-nanoseconds-between       (-> datetime/tz? datetime/tz? exact-integer?)]

 [datetime/tz=?                         (-> datetime/tz? datetime/tz? boolean?)]
 [datetime/tz<?                         (-> datetime/tz? datetime/tz? boolean?)]
 [datetime/tz>?                         (-> datetime/tz? datetime/tz? boolean?)]
 [datetime/tz<=?                        (-> datetime/tz? datetime/tz? boolean?)]
 [datetime/tz>=?                        (-> datetime/tz? datetime/tz? boolean?)]
 [datetime/tz-order                     order?])

(provide (rename-out [$datetime/tz datetime/tz]))
