#lang scheme
(define (cc amount)
  (define (iter count max-count total)
    (if (> count max-count)
        total
        (iter (+ count 1) max-count (+ total 
           
