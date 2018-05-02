#lang racket

(require (file "basic/basic.rkt"))

(define (compose f g)
  (lambda(x) (f (g x))))

;(define (repeated f n)
;  (if (= n 1) (lambda(x) (f x))
;      (lambda(x) (f ((repeated f (- n 1)) x) ))))
(define (repeated f n)
  (if (= n 1) f
      (compose f (repeated f (- n 1)) )))
;((repeated square 15) 5)


(define (smooth f)
  (lambda(x) (/ (+ (f (+ x dx)) (f (- x dx)) (f x)) 3)))

(define (smooth-n f n)
  (repeated (smooth f) n))

(define (average-damp-n f n)
  (repeated (average-damp f) n))

;(define (sqrt x)
;  (fixed-point (average-damp-n (lambda(y) (/ x y)) 3) 1.0))

 

(define (cube-root x)
  (fixed-point (lambda(y) (/ x (* y y)))  1.0))
(define (lg n)
  (cond ((>= n 2) (+ 1 (lg (/ n 2))))
        (else 0)))

(define (root x n)
  (if (= n 1) x
      (fixed-point (average-damp-n (lambda(y) (/ x (exp y (- n 1)))) (lg n)) 2.0 )))

(define (root-t x n)
  (if (= n 1) x
      (fixed-point (average-damp-n (lambda(y) (/ x (* y y y y))) 4) 1.0))) 

(define (iterate-improve improve-method good-enough)
  (lambda(x) (if (good-enough x) x
                 ((iterate-improve improve-method good-enough) (improve-method x)))))

(define (sqrt-i x)
  (define (good? guess)
    (< (abs (- (square guess) x)) 0.0001))
  (define (improve-guess guess)
    (average guess (/ x guess)))
  ((iterate-improve improve-guess good?) 1.0))
(sqrt-i 2)

(define (fixed-point-i f first-guess)
  (define (improve-f guess)
    (f guess))
  (define (good? guess)
    (close-enough? guess (f guess)))
  ((iterate-improve improve-f good?) 1.0))
(fixed-point-i cos 1.0)
(fixed-point cos 1.0)