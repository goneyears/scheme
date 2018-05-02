#lang scheme
(define (fast-expt b n)
  (define (square x)
    (* x x))
  (define (fast-expt-iter b product n counter)
    (cond ((= counter n) product)
          ((> (* 2 counter) n) (fast-expt-iter b (* b product) n (+ counter 1)))
          (else (fast-expt-iter b (square product) n (* 2 counter)))))
  (fast-expt-iter b b n 1))
 