#lang racket/base

(require racket/contract
         (only-in racket/function identity)
         racket/generic
         racket/list
         racket/match
         racket/serialize
         racket/string
         "private/math.rkt"
         "private/period/date-period-fields.rkt"
         "private/period/time-period-fields.rkt"
         "private/time/util.rkt"

         (for-syntax racket/base
                     syntax/parse
                     syntax/transformer))

;;;;
;; Units
(define temporal-units (append date-units time-units))
(define temporal-unit/c (apply symbols temporal-units))

;;;;
;; gen:equal+hash
(define (period-equal-proc x y fn)
  (match-define (Period dp1 tp1) x)
  (match-define (Period dp2 tp2) y)

  (and (fn dp1 dp2) (fn tp1 tp2)))

(define (period-hash-proc x fn)
  (match-define (Period dp tp) x)

  (bitwise-xor (fn dp) (fn tp)))

;;;;
;; gen:custom-write
(define (period-write-proc x out mode)
  (fprintf out (period->string x)))

(define (period->string x)
  (format "#<period ~a>" (period->member-string x)))

(define (period->member-string x)
  (match-define (Period
                 (date-period-fields years months weeks days)
                 (time-period-fields hours minutes seconds millis micros nanos))
    x)
  (define names '(year month week day hour minute second millisecond microsecond nanosecond))
  (define values (list years months weeks days hours minutes seconds millis micros nanos))

  (define strs
    (filter-map
     (λ (name value)
       (and (not (zero? value))
            (let ([suffix (if (= (abs value) 1) "" "s")])
              (format "~a ~a~a" value name suffix))))
     names
     values))

  (cond [(null? strs) "[empty]"]
        [else (string-append "of " (string-join strs ", "))]))

;;;;
;; Parsing
(define string->period
  (let ()
    (define (make-pattern-segments letters)
      (apply
       string-append
       (for/list ([letter (in-list letters)])
         (string-append "(?:([-+]?[0-9]+)" letter ")?"))))

    (define pattern
      (pregexp
       (string-append
        "([-+]?)P"
        (make-pattern-segments '("Y" "M" "W" "D"))
        "(T"
        (make-pattern-segments '("H" "M"))
        "(?:([-+]?[0-9]+)(?:[.,]([0-9]{0,9}))?S)?"
        ")?")))

    (define (! maybe-str)
      (if maybe-str
          (string->number maybe-str)
          0))

    (define (xform maybe-str)
      (if (equal? "-" maybe-str)
          period-negate
          identity))

    (define (nano maybe-str)
      (if maybe-str
          (string->number
           (substring (string-append maybe-str "000000000") 0 9))
          0))

    (λ (str)
      (match (regexp-match pattern str)
        [(list _
               (app xform f)
               (app ! y) (app ! mo) (app ! w) (app ! d)
               (not (== "T"))
               (app ! h) (app ! mi) (app ! s) (app nano n*))
         (define ns (if (< s 0) (- n*) n*))
         (f (period [years y] [months mo] [weeks w] [days d]
                    [hours h] [minutes mi] [seconds s] [nanoseconds ns]))]
        [_ #f]))))

;;;;
;; Predicates
(define (period? x)
  (Period? x))

(define (period-empty? x)
  (match-define (Period df tf) x)

  (and (date-period-fields-empty? df)
       (time-period-fields-empty? tf)))

(define (date-period? x)
  (match-define (Period _ tf) x)

  (time-period-fields-empty? tf))

(define (time-period? x)
  (match-define (Period df _) x)

  (date-period-fields-empty? df))

;;;;
;; List(like) operations
(define (period->list p [fields temporal-units])
  (for/list ([f (in-list fields)])
    (cons f (period-ref p f))))

(define (list->period xs)
  (for/fold ([p empty-period]) ([pair (in-list xs)])
    (period-set p (car pair) (cdr pair))))

;; Scaling
(define (period-scale p factor)
  (match-define (Period df tf) p)
  
  (Period (date-period-fields-scale df factor)
          (time-period-fields-scale tf factor)))

;;;;
;; Negation
(define (period-negate x)
  (match-define (Period df tf) x)

  (Period (date-period-fields-complement df)
          (time-period-fields-complement tf)))

;;;;
;; Dict-like
(define (period-ref x u)
  (match-define (Period df tf) x)
  
  (cond [(date-unit/c u) (date-period-fields-ref df u)]
        [(time-unit/c u) (time-period-fields-ref tf u)]))

(define (period-set x u v)
  (match-define (Period df tf) x)

  (cond [(date-unit/c u) (Period (date-period-fields-set df u v) tf)]
        [(time-unit/c u) (Period df (time-period-fields-set tf u v))]))

;;;;
;; Component constructors
(define-syntax-rule (define-date-component-constructors name ...)
  (begin
    (define (name n)
      (Period (date-period-fields-set empty-date-period-fields 'name n)
              empty-time-period-fields))
    ...))

(define-syntax-rule (define-time-component-constructors name ...)
  (begin
    (define (name n)
      (Period empty-date-period-fields
              (time-period-fields-set empty-time-period-fields 'name n)))
    ...))

(define-date-component-constructors years months weeks days)
(define-time-component-constructors hours minutes seconds milliseconds microseconds nanoseconds)

;;;;
;; Arithmetic
(define (period-add x y)
  (match-define (Period df1 tf1) x)
  (match-define (Period df2 tf2) y)

  (Period (date-period-fields-add df1 df2)
          (time-period-fields-add tf1 tf2)))

(define-syntax-rule (mk+ name ctor)
  (define (name x n) (period-add x (ctor n))))

(mk+ period-add-years years)
(mk+ period-add-months months)
(mk+ period-add-weeks weeks)
(mk+ period-add-days days)
(mk+ period-add-hours hours)
(mk+ period-add-minutes minutes)
(mk+ period-add-seconds seconds)
(mk+ period-add-milliseconds milliseconds)
(mk+ period-add-microseconds microseconds)
(mk+ period-add-nanoseconds nanoseconds)

(define (period-add-date-period t p)
  (match-define (Period df1 tf) t)
  (match-define (Period df2 _) p)
  (Period (date-period-fields-add df1 df2) tf))

(define (period-add-time-period t p)
  (match-define (Period df tf1) t)
  (match-define (Period _ tf2) p)
  (Period df (time-period-fields-add tf1 tf2)))

;;;;
;; Nanosecond conversion
(define (time-period->nanoseconds p)
  (match-define (Period _ (time-period-fields h m s ml mc n)) p)
  (+ (* h NS/HOUR)
     (* m NS/MINUTE)
     (* s NS/SECOND)
     (* ml NS/MILLI)
     (* mc NS/MICRO)
     n))

;;;;
;; Time period normalization
(define (time-period-normalize p [fields time-units])
  (define (field->divisor f)
    (case f
      [(hours) NS/HOUR]
      [(minutes) NS/MINUTE]
      [(seconds) NS/SECOND]
      [(milliseconds) NS/MILLI]
      [(microseconds) NS/MICRO]
      [(nanoseconds) 1]))
  
  (define-values (result _)
    (for/fold ([r empty-period] [ns (time-period->nanoseconds p)]) ([f (in-list time-units)])
      (cond [(memq f fields)
             (define divisor (field->divisor f))
             (define d (quotient ns divisor))
             (define m (remainder ns divisor))
             (values (period-set r f d) m)]
            [else
             (values r ns)])))

  result)

;;;;
;; Component accessors
(define (period->date-period p)
  (match-define (Period df _) p)
  (Period df empty-time-period-fields))

(define (period->time-period p)
  (match-define (Period _ tf) p)
  (Period empty-date-period-fields tf))

;;;;
;; Struct definition
(struct Period (dp tp)
  #:methods gen:equal+hash
  [(define equal-proc period-equal-proc)
   (define hash-proc  period-hash-proc)
   (define hash2-proc period-hash-proc)]

  #:methods gen:custom-write
  [(define write-proc period-write-proc)]

  #:property prop:serializable
  (make-serialize-info (λ (x)
                         (match-define (Period df tf) x)
                         (vector df tf))
                       #'deserialize-info:period
                       #f
                       (or (current-load-relative-directory)
                           (current-directory))))

(define deserialize-info:period
  (make-deserialize-info
   Period
   (λ () (error "period cannot have cycles"))))

(module+ deserialize-info
  (provide deserialize-info:period))

;;;;
;; Smart constructor
(define make-period
  (let ()
    (define (period . xs)
      (for/fold ([p empty-period]) ([x (in-list xs)])
        (period-add p x)))
    period))

(define-module-boundary-contract
  period
  make-period
  (->* () #:rest (listof period?) period?))

(define-match-expander $period
  (syntax-parser
    #:datum-literals (years months weeks days hours minutes seconds milliseconds microseconds nanoseconds)
    [(_ (~or (~optional [years yr:expr] #:defaults ([yr #'_]))
             (~optional [months mt:expr] #:defaults ([mt #'_]))
             (~optional [weeks wk:expr] #:defaults ([wk #'_]))
             (~optional [days ds:expr] #:defaults ([ds #'_]))
             (~optional [hours hr:expr] #:defaults ([hr #'_]))
             (~optional [minutes mn:expr] #:defaults ([mn #'_]))
             (~optional [seconds sc:expr] #:defaults ([sc #'_]))
             (~optional [milliseconds ms:expr] #:defaults ([ms #'_]))
             (~optional [microseconds mc:expr] #:defaults ([mc #'_]))
             (~optional [nanoseconds ns:expr] #:defaults ([ns #'_])))
        ...)
     #'(Period (date-period-fields yr mt wk ds)
               (time-period-fields hr mn sc ms mc ns))])
  (make-variable-like-transformer #'period))

;;;;
;; Constants
(define empty-period (Period empty-date-period-fields empty-time-period-fields))

;;;;
;; Exports
(provide/contract
 [period?                  (-> any/c boolean?)]
 [period-empty?            (-> period? boolean?)]
 [date-period?             (-> period? boolean?)]
 [time-period?             (-> period? boolean?)]
 [period-negate            (-> period? period?)]
 [period-scale             (-> period? exact-integer? period?)]

 [period->date-period      (-> period? date-period?)]
 [period->time-period      (-> period? time-period?)]
 [time-period->nanoseconds (-> time-period? exact-integer?)]

 [empty-period             (and/c date-period? time-period? period?)]

 [period-ref               (-> period? temporal-unit/c exact-integer?)]
 [period-set               (-> period? temporal-unit/c exact-integer? period?)]

 [period-add-years         (-> period? exact-integer? period?)]
 [period-add-months        (-> period? exact-integer? period?)]
 [period-add-weeks         (-> period? exact-integer? period?)]
 [period-add-days          (-> period? exact-integer? period?)]
 [period-add-date-period   (-> period? date-period? period?)]

 [period-add-hours         (-> period? exact-integer? period?)]
 [period-add-minutes       (-> period? exact-integer? period?)]
 [period-add-seconds       (-> period? exact-integer? period?)]
 [period-add-milliseconds  (-> period? exact-integer? period?)]
 [period-add-microseconds  (-> period? exact-integer? period?)]
 [period-add-nanoseconds   (-> period? exact-integer? period?)]
 [period-add-time-period   (-> period? time-period? period?)]

 [time-period-normalize    (->* (time-period?) ((listof temporal-unit/c)) time-period?)]

 [period->list             (->* (period?)
                                ((listof temporal-unit/c))
                                (listof (cons/c temporal-unit/c exact-integer?)))]
 [list->period             (-> (listof (cons/c temporal-unit/c exact-integer?)) period?)]

 [years                    (-> exact-integer? date-period?)]
 [months                   (-> exact-integer? date-period?)]
 [weeks                    (-> exact-integer? date-period?)]
 [days                     (-> exact-integer? date-period?)]

 [hours                    (-> exact-integer? time-period?)]
 [minutes                  (-> exact-integer? time-period?)]
 [seconds                  (-> exact-integer? time-period?)]
 [milliseconds             (-> exact-integer? time-period?)]
 [microseconds             (-> exact-integer? time-period?)]
 [nanoseconds              (-> exact-integer? time-period?)]

 [date-units               (listof symbol?)]
 [time-units               (listof symbol?)]
 [temporal-units           (listof symbol?)]
 [date-unit/c              flat-contract?]
 [time-unit/c              flat-contract?]
 [temporal-unit/c          flat-contract?])

(provide (rename-out [$period period]))
