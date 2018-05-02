#lang racket
(require (file "C:/Users/Mach/Files/scheme/33.rkt"))

(define (div3? num)
  (define (try i)
    (cond ((> (* i 3) num) #f)
          ((= (* i 3) num) #t)
          (else (try (+ i 1)))))
  (try 1))



(define (D i)
  (cond ((div3? (+ i 1)) (* (/ (+ i 1) 3) 2))
        (else 1)))
(define (N i)
  1.0)
(+ 2 (cont-fract N D 31))


