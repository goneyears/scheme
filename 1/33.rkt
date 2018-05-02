#lang racket
(require (file "C:/Users/Mach/Files/scheme/basic/basic.rkt"))
(provide cont-fract)
(define (cont-fract n d k)
  (define (cont i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (cont (+ i 1))))))
  (cont 1))



(define (cont-fract-iter n d k)
  (define (iter ek i)
    (if (= i k)
        ek
        (iter (/ (n (- k i)) (+ (d (- k i)) ek)) (+ i 1))))
  (iter 1 0))
