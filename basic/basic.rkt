#lang racket
(provide square close-enough? average inc dx fixed-point average-damp exp close-enough? displays)
(define (square x)
  (* x x))
(define (average x y)
  (/ (+ x y) 2))
(define tolerance 0.00000000000001)
  (define (close-enough? a b)
    (if (< (abs (- a b)) tolerance)
        true
        false))
(define (fixed-point f first-guess)
  (define (try f guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try f next ))))
  (try f first-guess)
  )
(define (average-damp f)
  (lambda(x) (average (f x) x)))
(define (inc x)
  (+ x 1))

(define dx 0.000001)

(define (exp x n)
  (if (= n 0) 1
      (* x (exp x (- n 1)))))

  
(define (displays . messages)
  (define (display-iter messagelists)
    (cond ((> (length messagelists) 0)
              (display (car messagelists))
              (newline)
              (display-iter (cdr messagelists)))))
  (display-iter messages)
  )

