#lang scheme
(define (square i)
  (* i i))
(define (f g)
  (g 2))
(f f)