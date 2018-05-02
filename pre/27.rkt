#lang scheme
(define (sum start end)
  (define (sum-iter res count)
    (if (= count (+ end 1)) res
        (sum-iter (+ res 1) (+ count 1))))
  (sum-iter 0 start))
(sum 1 2)
  