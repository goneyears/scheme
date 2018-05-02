#lang scheme
(define (f n)
  (if (<= n 3)
      n
      (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))) )))
      
(define (ff n)
  (define (ff-iter n counter ret_1 ret_2 ret_3)
    (cond ((= n 1) 1)
          ((= n 2) 2)
          ((= n 3) 3)
          ((= counter n) ret_1)
          (else   (ff-iter n (+ counter 1)  (+ ret_1 (* 2 ret_2) (* 3 ret_3)) ret_1 ret_2))))

  (ff-iter n 3 3 2 1))
  