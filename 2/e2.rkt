#lang racket
;5 2 1
(define (count-coins n)
  (+ (count-coins (- n 5))
     + (count-coins (- n 2))