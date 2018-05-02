#lang scheme
(define (square x)
  (* x x))
(define (not-equal a b)
  (not (= a b)))
(define (expmod a exp n)
  (cond ((= exp 0) 1)
        ( (even? exp) (r-check (expmod a (/ exp 2) n)  n) )
        ( (odd? exp)  (remainder (* a (expmod a (- exp 1) n)) n))))
(define (r-c a r n)
  (if (and (not-equal a 1) (not-equal a (- n 1)) (= r 1)) 0 r))

(define (r-check x n)
  (r-c x (remainder (square x) n) n))

(define (ex-expmod a n)
  (expmod a (- n 1) n))

(define (ex-expmod-iter res n count)
  (cond ((= count  n ) true )
        ((not-equal  res 1) false)
        (else (ex-expmod-iter (ex-expmod (+ count 1) n) n (+ count 1)))))
(define (fast-fermat n)
  (ex-expmod-iter 1 n 1))
(ex-expmod 2 17)
;(r-check a (expmod a (/ exp 2)) n)
     
        