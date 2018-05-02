#lang scheme
(define (square i)
  (* i i))
(define (inc i)
  (+ i 1))
(define (cube a)  (* a a a))
(define (even? n)
  (= (remainder n 2) 0))
(define (odd? n)
  (not (even? n)))
(define (gcd a b)
  (if (= b 0) a
      (gcd b (remainder a b))))
(gcd 7 6)
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divide? n test-divisor) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
               
(define (divide? n m)
  (= (remainder n m) 0) ) 
  
(define (smallest-divisor n)
  (find-divisor n 2))
  
(define (prime? n)
  (if (= n 1) false  (= (smallest-divisor n) n)))
(define (elem i)
  (/ (* (* 2 i) (* 2 (+ i 1))) (square (+ (* 2 i) 1))))
(define (product term a next b)
  (if (> a b) 1
      (* (term a) (product term (next a) next b))))
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b) result
        (iter (next a) (* (term a) result))))
  (iter a 1))
(define (product-iter term a next b)
  (if (> a b) 1
      (* (term a) (product-iter term (next a) next b))))
(define (ex-prime a b)
  (if (= (gcd a b) 1) true false))
(define (accumulate combiner null-value term a next b)
  (if (> a b) null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))
(define (accumulate-filter filter combiner null-value term a next b)
  (if (> a b) null-value
      (if (filter  a)
          (combiner (term a) (accumulate-filter filter combiner null-value term (next a) next b))
          (combiner null-value (accumulate-filter filter combiner null-value term (next a) next b)))))
(define (normal x) x)
(define (product-prime n)
  (define (term-prime? a) (ex-prime a n))
  (accumulate-filter term-prime? * 1 normal 1 inc n))
(product-prime 15)
    

(accumulate-filter prime? + 0 normal 1 inc 15)     
;(accumulate-filter ex-prime * 1 normal 1 inc 5)
(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b) result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))


(define (sum-com term a next b)
  (accumulate + 0 term a next b))
(define (product-com term a next b)
  (accumulate-iter * 1 term a next b))
(define (pi4 n)
  (product-com elem 1 inc n))
;(* (pi4 1000) 4.0)