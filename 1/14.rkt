#lang scheme
; a*b=(2*a)*(b/2) even?b
; a*b=a+a*(b-1) odd?b


  (define (double x)
    (+ x x))
  (define (halve x)
    (/ x 2))
  (define (even? x)
    (= (remainder x 2) 0))
  (define (odd? x)
    (= (remainder x 2) 1))
(define (fast-mul a b)
  (cond ((= b 1) a)
        ((= b 0) 0)
        ((even? b)  (fast-mul (double a) (halve b)))
        ((odd? b) (+ a (fast-mul a (- b 1))))))

(define (ff-mul a b)
  (define (ff-iter product mulb counter)
    (cond ((= counter 0) product)
          ((even? counter)   (ff-iter product (double mulb) (halve counter)))
          ((odd? counter)    (ff-iter (+ product mulb) mulb (- counter 1)))))
  (ff-iter 0 b a))
             

            
     