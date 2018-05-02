#lang racket

(require (file "C:/Users/Mach/Files/scheme/basic/basic.rkt"))
(require (file "C:/Users/Mach/Files/scheme/33.rkt"))


(define (tan-cf x k)
  (define (N i)
    (if (= i 1) x
        (- (square x))))
  (define (D i)
    (- (* 2 i) 1))
  (cont-fract N D k))

(tan-cf (/ pi 3.0) 11)

  