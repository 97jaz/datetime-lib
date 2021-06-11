#lang racket/base

(require racket/contract
         racket/format
         racket/generic
         racket/match
         racket/serialize
         data/order
         "../date.rkt"
         "../datetime.rkt"
         "../time.rkt"
         "../zone.rkt"
         "../private/compare.rkt"
         "../private/date/contract.rkt"
         "../private/time/util.rkt"

         (for-syntax racket/base
                     syntax/transformer))

;;;;
;; gen:equal+hash
(define (datetime/offset-equal-proc x y fn)
  (match-define (Datetime/offset dt1 o1) x)
  (match-define (Datetime/offset dt2 o2) y)
  (and (fn dt1 dt2) (fn o1 o2)))

(define (datetime/offset-hash-proc x fn)
  (match-define (Datetime/offset dt o) x)
  (bitwise-xor (fn dt) (fn o)))

;;;;
;; gen:custom-write
(define (datetime/offset-write-proc x out mode)
  (if mode
      (fprintf out "#<datetime/offset ~a>" (datetime/offset->string x))
      (display (datetime/offset->string x) out)))

(define (datetime/offset->string x)
  (match-define (Datetime/offset dt off) x)
  (format "~a~a"
          (datetime->iso8601 dt)
          (utc-offset->string off)))

(define (utc-offset->string off)
  (cond [(zero? off)
         "Z"]
        [else
         (define total (abs off))
         (define hours (quotient total (* 60 60)))
         (define minutes (remainder (quotient total 60) 60))
         (define seconds (remainder total 60))
         (format "~a~a:~a~a~a"
                 (if (negative? off) "-" "+")
                 (~r hours #:min-width 2 #:pad-string "0" #:sign #f)
                 (~r minutes #:min-width 2 #:pad-string "0" #:sign #f)
                 (if (zero? seconds) "" ":")
                 (if (zero? seconds) "" (~r seconds #:min-width 2 #:pad-string "0" #:sign #f)))]))

(define (datetime/offset->iso8601 x)
  (match-define (Datetime/offset dt off) x)
  (format "~a~a"
          (datetime->iso8601 dt)
          (let ([seconds (remainder off 60)])
            (utc-offset->string (- off seconds)))))

;;;;
;; UTC offset adjustment
(define (datetime/offset-at-utc-offset x [off (datetime/offset->utc-offset x)])
  (match x
    [(Datetime/offset _ (== off))
     x]
    
    [(Datetime/offset dt orig-off)
     (define diff (- off orig-off))
     (Datetime/offset (datetime-add-nanoseconds dt (* diff NS/SECOND)) off)]))

;;;;
;; Predicate
(define (datetime/offset? x)
  (Datetime/offset? x))

;;;;
;; Datetime accessors
(define (datetime/offset->utc-offset x)
  (match-define (Datetime/offset _ off) x)
  off)

(define (datetime/offset->utc-datetime x)
  (match-define (Datetime/offset dt _) (datetime/offset-at-utc-offset x 0))
  dt)

(define (datetime/offset->local-datetime x)
  (match-define (Datetime/offset dt _) x)
  dt)

(define (datetime/offset->date x)
  (match-define (Datetime/offset dt _) x)
  (datetime->date dt))

(define (datetime/offset->time x)
  (match-define (Datetime/offset dt _) x)
  (datetime->time dt))

;;;;
;; POSIX & Julian conversion
(define (datetime/offset->posix x)
  (datetime->posix (datetime/offset->utc-datetime x)))

(define (datetime/offset->jd t)
  (datetime->jd (datetime/offset->utc-datetime t)))

(define (posix->datetime/offset x #:offset offset)
  (Datetime/offset (datetime-add-nanoseconds (posix->datetime x) (* NS/SECOND offset))
                   offset))

(define (jd->datetime/offset x #:offset offset)
  (Datetime/offset (datetime-add-nanoseconds (jd->datetime x) (* NS/SECOND offset))
                   offset))

;;;;
;; Arithmetic
(define (datetime/offset-add-months t n)
  (match-define (Datetime/offset dt off) t)
  (Datetime/offset (datetime-add-months dt n) off))

(define (datetime/offset-add-days t n)
  (match-define (Datetime/offset dt off) t)
  (Datetime/offset (datetime-add-days dt n) off))

(define (datetime/offset-add-nanoseconds t n)
  (match-define (Datetime/offset dt off) t)
  (Datetime/offset (datetime-add-nanoseconds dt n) off))

;;;;
;; Difference
(define-syntax-rule (define-between name fn)
  (define (name x y)
    (match-define (Datetime/offset dt1 o) x)
    (match-define (Datetime/offset dt2 _) (datetime/offset-at-utc-offset y o))
    (fn dt1 dt2)))

(define-between datetime/offset-months-between      datetime-months-between)
(define-between datetime/offset-days-between        datetime-days-between)
(define-between datetime/offset-nanoseconds-between datetime-nanoseconds-between)

;;;;
;; Struct definition
(struct Datetime/offset (dt off)
  #:methods gen:equal+hash
  [(define equal-proc datetime/offset-equal-proc)
   (define hash-proc datetime/offset-hash-proc)
   (define hash2-proc datetime/offset-hash-proc)]

  #:methods gen:custom-write
  [(define write-proc datetime/offset-write-proc)]

  #:property prop:serializable
  (make-serialize-info (λ (t)
                         (match-define (Datetime/offset dt off) t)
                         (vector dt off))
                       #'deserialize-info:datetime/offset
                       #f
                       (or (current-load-relative-directory)
                           (current-directory))))

(define deserialize-info:datetime/offset
  (make-deserialize-info
   Datetime/offset
   (λ () (error "datetime/offset cannot have cycles"))))

(module+ deserialize-info
  (provide deserialize-info:datetime/offset))

;;;;
;; Smart constructor
(define make-datetime/offset
  (let ()
    (define (datetime/offset year [month 1] [day 1] [hours 0] [minutes 0] [seconds 0] [nanos 0]
                             #:offset offset)
      (Datetime/offset (datetime year month day hours minutes seconds nanos)
                       offset))
    datetime/offset))

(define-module-boundary-contract
  datetime/offset
  make-datetime/offset
  (->i ([year exact-integer?] #:offset [offset utc-offset/c])
       ([month (integer-in 1 12)]
        [day (year month) (day-of-month/c year month)]
        [hours (integer-in 0 23)]
        [minutes (integer-in 0 59)]
        [seconds (integer-in 0 59)]
        [nanoseconds (integer-in 0 (sub1 NS/DAY))])
       [dt datetime/offset?]))

(define-match-expander $datetime/offset
  (syntax-rules ()
    [(_ y mo d h mi s n off)
     (Datetime/offset (datetime (date y mo d) (time h mi s n)) off)]
    [(_ d t off)
     (Datetime/offset (datetime d t) off)]
    [(_ dt off)
     (Datetime/offset dt off)])
  (make-variable-like-transformer #'datetime/offset))

;;;;
;; Other constructors
(define (datetime->datetime/offset dt #:offset offset)
  (Datetime/offset dt offset))

;;;;
;; Comparison
(define (datetime/offset-comparator x y)
  (match-define (Datetime/offset dt1 off) x)
  (match-define (Datetime/offset dt2 _) (datetime/offset-at-utc-offset y off))

  (datetime-order dt1 dt2))

(match-define (comparison datetime/offset=?
                          datetime/offset<?
                          datetime/offset<=?
                          datetime/offset>?
                          datetime/offset>=?
                          datetime/offset-order)
  (build-comparison 'datetime/offset-order datetime/offset? datetime/offset-comparator))

;;;;
;; Exports
(provide/contract
 [datetime/offset?                     (-> any/c boolean?)]

 [datetime/offset->date                (-> datetime/offset? date?)]
 [datetime/offset->time                (-> datetime/offset? time?)]
 [datetime/offset->utc-offset          (-> datetime/offset? utc-offset/c)]
 
 [datetime/offset-at-utc-offset        (->* (datetime/offset?) (utc-offset/c) datetime/offset?)]
 
 [datetime->datetime/offset            (->i ([dt datetime?] #:offset [offset utc-offset/c])
                                            [r datetime/offset?])]

 [datetime/offset->posix               (-> datetime/offset? rational?)]
 [datetime/offset->jd                  (-> datetime/offset? rational?)]

 [posix->datetime/offset               (->i ([sec rational?] #:offset [offset utc-offset/c])
                                            [r datetime/offset?])]
 [jd->datetime/offset                  (->i ([sec rational?] #:offset [offset utc-offset/c])
                                            [r datetime/offset?])]
  
 [datetime/offset->iso8601             (-> datetime/offset? string?)] 
 [datetime/offset->string              (-> datetime/offset? string?)]

 [datetime/offset->utc-datetime        (-> datetime/offset? datetime?)]
 [datetime/offset->local-datetime      (-> datetime/offset? datetime?)]

 [datetime/offset-add-months           (-> datetime/offset? exact-integer? datetime/offset?)]
 [datetime/offset-add-days             (-> datetime/offset? exact-integer? datetime/offset?)]
 [datetime/offset-add-nanoseconds      (-> datetime/offset? exact-integer? datetime/offset?)]

 [datetime/offset-months-between       (-> datetime/offset? datetime/offset? exact-integer?)] 
 [datetime/offset-days-between         (-> datetime/offset? datetime/offset? exact-integer?)]
 [datetime/offset-nanoseconds-between  (-> datetime/offset? datetime/offset? exact-integer?)]

 [datetime/offset=?                    (-> datetime/offset? datetime/offset? boolean?)]
 [datetime/offset<?                    (-> datetime/offset? datetime/offset? boolean?)]
 [datetime/offset>?                    (-> datetime/offset? datetime/offset? boolean?)]
 [datetime/offset<=?                   (-> datetime/offset? datetime/offset? boolean?)]
 [datetime/offset>=?                   (-> datetime/offset? datetime/offset? boolean?)]
 [datetime/offset-order                order?])

(provide (rename-out [$datetime/offset datetime/offset]))
