#lang scheme
(define (cube a)  (* a a a))
(define (even? n)
  (= (remainder n 2) 0))
(define (odd? n)
  (not (even? n)))
(define (sum term a next b)
  (if (> a b) 0
      (+ (term a) (sum term (next a) next b))))

(define (inc i)
  (+ i 1))
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b) result
        (iter (next a) (+ (term a) result))))
  (iter a 0))
(define (simpson f a b n)
  (define (factor i)
    (cond ((or (= i 0) (= i n)) 1)
          ((odd? i) 4)
          (else 2)))
  (define h (/ (- b a) n))
          
  (define (y i)
    (f (+ a (* i h))))
  (define (term i)
    (* (factor i) (y i)))
  (* (/ h 3) (sum-iter term 0 inc n))
  )

 (simpson cube 0 1 100)
