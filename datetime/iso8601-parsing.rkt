#lang racket/base

(require racket/contract/base
         (only-in racket/math exact-floor)
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc
         "date.rkt"
         "datetime.rkt"
         "datetime/offset.rkt"
         "datetime/tz.rkt"
         "exn.rkt"
         "generic/date-provider.rkt"
         "generic/time-provider.rkt"
         "generic/datetime-provider.rkt"
         "generic/utc-offset-provider.rkt"
         "generic/tz-provider.rkt"
         "private/time/util.rkt"
         "time.rkt")

(provide/contract
 [iso8601->date            (-> string? date?)]
 [iso8601->time            (-> string? time?)]
 [iso8601->datetime        (-> string? datetime?)]
 [iso8601->datetime/offset (-> string? datetime/offset?)]
 [iso8601/tz->datetime/tz  (-> string? datetime/tz?)])

(define-tokens data-tokens (YEAR D2 FRACTION TZID))
(define-empty-tokens empty-tokens (DASH PLUS COLON T Z EOF))

(define parse-temporal
  (parser
   [tokens data-tokens empty-tokens]
   [error (λ (tok-ok? tok-name tok-value)
            (raise-iso8601-parse-error))]
   [start temporal]
   [end EOF]
   [grammar
    (temporal
     [(datetime/tz)     $1]
     [(datetime/offset) $1]
     [(datetime)        $1]
     [(time)            $1]
     [(date)            $1])
    
    (date
     [(optsign YEAR DASH month DASH day)  (date ($1 $2) $4 $6)]
     [(optsign YEAR DASH month)           (date ($1 $2) $4)]
     [(optsign YEAR)                      (date ($1 $2))])
    
    (time
     [(hour)                              (time $1)]
     [(hour COLON min)                    (time $1 $3)]
     [(hour COLON min COLON sec)          (time $1 $3 $5)]
     [(hour COLON min COLON sec fraction) (time $1 $3 $5 $6)])

    (datetime
     [(date T time)                       (date->datetime $1 $3)])

    (datetime/offset
     [(datetime offset) (datetime->datetime/offset $1 #:offset $2)])

    (datetime/tz
     [(datetime offset TZID)
      (let ([dto (datetime->datetime/offset $1 #:offset $2)])
        (datetime/offset->datetime/tz dto #:tz $3))])

    (offset
     [(sign hour)           ($1 (* 3600 $2))]
     [(sign hour COLON min) ($1 (+ (* 3600 $2) (* 60 $4)))]
     [(Z)                   0])

    (month
     [(D2) (guard $1 (integer-in 1 12))])

    (day
     [(D2) (guard $1 (integer-in 1 31))])

    (hour
     [(D2) (guard $1 (integer-in 0 23))])

    (min
     [(D2) (guard $1 (integer-in 0 59))])

    (sec
     [(min) $1])

    (sign
     [(PLUS)  +]
     [(DASH)  -])

    (optsign
     [(sign)  $1]
     [()      +])

    (fraction
     [(FRACTION) (exact-floor (* $1 NS/SECOND))])]))

(define (make-parser kind p? xform)
  (λ (str)
    (define in (open-input-string str))
    (define t (parse-temporal (λ () (scan in))))

    (if (p? t)
        (xform t)
        (raise-iso8601-parse-error
         (format "Unable to parse as a ~a: ~a" kind str)))))

(define iso8601->date            (make-parser 'date date-provider? ->date))
(define iso8601->time            (make-parser 'time time-provider? ->time))
(define iso8601->datetime        (make-parser 'datetime datetime-provider? ->datetime))
(define iso8601->datetime/offset (make-parser 'datetime/offset utc-offset-provider? ->datetime/offset))
(define iso8601/tz->datetime/tz  (make-parser 'datetime/tz tz-provider? ->datetime/tz))

(define (guard val ok?)
  (if (ok? val)
      val
      (raise-iso8601-parse-error)))

(define scan
  (lexer
   [(:>= 4 digit) (token-YEAR (string->number lexeme))]
   [(:= 2 digit)  (token-D2 (string->number lexeme))]
   [(:: (:or "." ",") (:+ digit)) (token-FRACTION (string->number (regexp-replace #rx"," lexeme ".")))]
   ["-" (token-DASH)]
   ["+" (token-PLUS)]
   [":" (token-COLON)]
   ["T" (token-T)]
   ["Z" (token-Z)]
   ["[" (token-TZID (list->string (scan-tzid input-port)))]
   [(eof) (token-EOF)]
   [any-char (raise-iso8601-parse-error)]))

(define scan-tzid
  (lexer
   ["]"
    '()]
   [(:~ (:or "]" " "))
    (cons (string-ref lexeme 0)
          (scan-tzid input-port))]
   [any-char
    (raise-iso8601-parse-error)]
   [(eof)
    (raise-iso8601-parse-error)]))

(define-lex-abbrev digit (:/ #\0 #\9))

(define (raise-iso8601-parse-error [msg "Unable to parse input as an ISO 8601 string"])
  (raise
   (exn:datetime:parse msg
                       (current-continuation-marks))))