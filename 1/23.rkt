#lang scheme

((double inc) 1)
(define (doub f)
  (lambda (x) (f (f x))))
((doub inc) 1)