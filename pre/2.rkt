#lang scheme
(define (cube a)
  (* a a a ))
(define (sum a b term next )
  (if (> a b)
      0
      (+ (term a)
         (sum (next a) b term  next))))
(define (inc a )
  (+ a 1))
(define (pi-term x)
  (/ 1.0 (* x (+ x 2))))
(define (pi-next a)
  (+ a 4))
(define (pi x)
  (* 8
  (sum 1 x pi-term  pi-next)))