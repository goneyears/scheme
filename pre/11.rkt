#lang scheme
(define (pascal n m)
  (cond ((= m 0) 0)
        ((= m 1) 1)
        ((= m n) 1)
        ((> m n) 0)
        (else   (+ (pascal (- n 1) (- m 1)) (pascal (- n 1) m)))))

