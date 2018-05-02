#lang scheme

(define (try counter)
  (random 1)
  (if (= counter 0) 0
      (try (- counter 1)))
  )
(try 10)