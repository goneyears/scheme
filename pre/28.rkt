#lang scheme
(define (cube a)
  (* a a a))
;(define (sum-cube a b)
;  (if (> a b) 0
;      (+ (cube a) (sum-cube (+ a 1) b))))
;(sum-cube 1 2)
(define (sum term a next b)
  (if (> a b) 0
      (+ (term a) (sum term (next a) next b))))
(define (inc a)  (+ a 1))
(define (sum-cube-adv a b)
  (sum cube a inc b))
(sum-cube-adv 1 2)

(define (ele-integral f a b dx)
  (define (add-dx x) (+ x  dx ))
  (* (sum f (+ a (/ dx 2)) add-dx b) dx))

(define (integral f a b n)
  (define (dinc i) (+ i 2))
  (define (h1) (/ (- b a) n))
  (define (x i) (+ a (* i (h1))))
  (define (H i)
    (+ (* 4 (f (x i))) (* 2 (f (x (+ i 1))))))
  (define (y0) (f a))
  (* (/ (h1) 3) (+ (y0) (sum H  1 dinc (- n 1)))))
(define (f x)  (cube x))
(define (abs x)
  (if (> x 0) x
      (- x)))

(abs (- 0.25 (integral cube 0 1.0 10)))
(abs (- 0.25 (ele-integral cube 0 1 0.1)))

(define (simpson f a b n)
    
    (define h (/ (- b a) n))

    (define (y k)
        (f (+ a (* k h))))

    (define (factor k)
        (cond ((or (= k 0) (= k n))
                1)
              ((odd? k)
                4)
              (else
                2)))
    
    (define (term k)
        (* (factor k)
           (y k)))

    (define (next k)
        (+ k 1))

    (if (not (even? n))
        (error "n can't be odd")
        (* (/ h 3)
           (sum term (exact->inexact 0) next n))))

(simpson cube 0 1 100)