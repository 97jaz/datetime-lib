#lang racket/base

(require data/order)

(provide (all-defined-out))

(struct comparison (=? <? <=? >? >=? order))

(define (build-comparison name pred? comparator)
  (define (=? x y) (eq? '= (comparator x y)))
  (define (<? x y) (eq? '< (comparator x y)))
  (define (>? x y) (eq? '> (comparator x y)))
  
  (define (<=? x y)
    (case (comparator x y)
      [(< =) #t]
      [else  #f]))
  
  (define (>=? x y)
    (case (comparator x y)
      [(> =) #t]
      [else  #f]))
  
  (define the-order (order name pred? comparator))
  
  (comparison =? <? <=? >? >=? the-order))
