#lang racket/base

(require racket/contract
         racket/format
         racket/generic
         racket/match
         racket/serialize
         data/order
         "period.rkt"
         "private/compare.rkt"
         "private/math.rkt"
         "private/time/util.rkt"

         (for-syntax racket/base
                     syntax/transformer))

;;;;
;; Nanosecond conversion
(define (time->day-ns t)
  (match-define (Time h m s n) t)
  (+ (* h NS/HOUR)
     (* m NS/MINUTE)
     (* s NS/SECOND)
     n))

(define (day-ns->time ns)
  (let* ([h (quotient ns NS/HOUR)]
         [ns (- ns (* h NS/HOUR))]
         [m (quotient ns NS/MINUTE)]
         [ns (- ns (* m NS/MINUTE))]
         [s (quotient ns NS/SECOND)]
         [ns (- ns (* s NS/SECOND))])
    (Time h m s ns)))

;;;;
;; Predicate
(define (time? x)
  (Time? x))

;;;;
;; Accessors
(define (time->hours x)
  (Time-h x))

(define (time->minutes x)
  (Time-m x))

(define (time->seconds x [fractional? #f])
  (+ (Time-s x)
     (if fractional?
         (/ (Time-n x) NS/SECOND)
         0)))

(define (time->milliseconds x) (div (Time-n x) NS/MILLI))
(define (time->microseconds x) (div (Time-n x) NS/MICRO))
(define (time->nanoseconds x) (Time-n x))

;;;;
;; Arithmetic
(define (time-add-nanoseconds t ns)
  (day-ns->time
   (mod (+ (time->day-ns t) ns)
        NS/DAY)))

;;;;
;; Difference
(define (time-nanoseconds-between x y)
  (- (time->day-ns y) (time->day-ns x)))

;;;;
;; ISO 8601 formatting
(define (time->iso8601 t)
  (define (f n l) (~r n #:min-width l #:pad-string "0"))
  
  (match-define (Time h m s n) t)
  (define fsec (+ s (/ n NS/SECOND)))
  (define pad (if (>= s 10) "" "0"))

  (format "~a:~a:~a~a" (f h 2) (f m 2) pad (~r fsec #:precision 9)))

;;;;
;; gen:equal+hash
(define (time-equal-proc x y _)
  (match-define (Time h1 m1 s1 n1) x)
  (match-define (Time h2 m2 s2 n2) y)
  (and (= h1 h2)
       (= m1 m2)
       (= s1 s2)
       (= n1 n2)))

(define (time-hash-proc x _)
  (time->day-ns x))

(define (time-hash2-proc x _)
  (match-define (Time h m s n) x)
  (bitwise-and n
               (arithmetic-shift s 30)
               (arithmetic-shift m 36)
               (arithmetic-shift h 42)))

;;;;
;; gen:custom-write
(define (time-write-proc t out mode)
  (if mode
      (fprintf out "#<time ~a>" (time->iso8601 t))
      (display (time->iso8601 t) out)))

;;;;
;; struct definition
(struct Time (h m s n)
  #:methods gen:equal+hash
  [(define equal-proc time-equal-proc)
   (define hash-proc  time-hash-proc)
   (define hash2-proc time-hash2-proc)]
  
  #:methods gen:custom-write
  [(define write-proc time-write-proc)]
  
  #:property prop:serializable
  (make-serialize-info (λ (t)
                         (match-define (Time h m s n) t)
                         (vector h m s n))
                       #'deserialize-info:time
                       #f
                       (or (current-load-relative-directory)
                           (current-directory))))

(define deserialize-info:time
  (make-deserialize-info
   Time
   (λ () (error "time cannot have cycles"))))

(module+ deserialize-info
  (provide deserialize-info:time))

;;;;
;; Smart constructor
(define make-time
  (let ()
    (define (time h [m 0] [s 0] [n 0])
      (Time h m s n))
    time))

(define-module-boundary-contract
  time
  make-time
  (->i ([hours (integer-in 0 23)])
       ([minutes (integer-in 0 59)]
        [seconds (integer-in 0 59)]
        [nanoseconds (integer-in 0 (sub1 NS/SECOND))])
       [t time?]))

(define-match-expander $time
  (syntax-rules ()
    [(_ h m s n) (Time h m s n)])
  (make-variable-like-transformer #'time))

;;;;
;; Comparison
(define (time-comparator x y)
  (match (- (time->day-ns x) (time->day-ns y))
    [(? negative?) '<]
    [(? zero?)     '=]
    [_             '>]))

(match-define (comparison time=? time<? time<=? time>? time>=? time-order)
  (build-comparison 'time-order time? time-comparator))

;;;;
;; Constants
(define MIDNIGHT (Time 0 0 0 0))
(define NOON (Time 12 0 0 0))

;;;;
;; Exports
(provide/contract
 [time? (-> any/c boolean?)]

 [time->hours        (-> time? (integer-in 0 23))]
 [time->minutes      (-> time? (integer-in 0 59))]
 [time->seconds      (->* (time?) (boolean?) (and/c rational? (>=/c 0) (</c 60)))]
 [time->milliseconds (-> time? (integer-in 0 999))]
 [time->microseconds (-> time? (integer-in 0 999999))]
 [time->nanoseconds  (-> time? (integer-in 0 999999999))]

 [time->iso8601 (-> time? string?)]

 [time=?     (-> time? time? boolean?)]
 [time<?     (-> time? time? boolean?)]
 [time>?     (-> time? time? boolean?)]
 [time<=?    (-> time? time? boolean?)]
 [time>=?    (-> time? time? boolean?)]
 [time-order order?]

 [time->day-ns (-> time? (integer-in 0 (sub1 NS/DAY)))]
 [day-ns->time (-> (integer-in 0 (sub1 NS/DAY)) time?)]

 [time-add-nanoseconds     (-> time? exact-integer? time?)]

 [time-nanoseconds-between (-> time? time? exact-integer?)]

 [MIDNIGHT time?]
 [NOON     time?])

(provide (rename-out [$time time]))
