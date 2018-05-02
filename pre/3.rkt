#lang scheme
(define (sum term a next b )
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
(define (sum-inc a b)
  (sum (lambda(x) x)
       a
       (lambda(x) (+ x 1))
       b))
(define (sum-cube a b)
  (sum (lambda(x) (* x x x))
       a
       (lambda(x) (+ x 1))
       b))

         

       
         
