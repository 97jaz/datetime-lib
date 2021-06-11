#lang racket/base

(require racket/string)

(provide (all-defined-out))

(define (abstract-period->member-string names values)
  (cond [(andmap zero? values)
         #f]
        [else
         (define non-empty
           (for/list ([name (in-list names)]
                      [value (in-list values)]
                      #:unless (zero? value))
             (format "~a ~a~a" value name (if (= 1 (abs value)) "" "s"))))
         (string-join non-empty ", ")]))
