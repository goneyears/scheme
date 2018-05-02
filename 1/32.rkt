#lang scheme
;(define (delay exp)
;  (lambda () exp))
(define-syntax delay
  (syntax-rules ()
    ((_ exp) (lambda () exp))))

(define (force delayed-obj)
  (delayed-obj))

;(define (cons-stream a b) (cons a (delay b)))
(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(cons 1 (display 'hey))

(define (foo x)
  (display "foo: ") (write x) (newline)
  x)

(cons-stream 1 (foo 2))

(define (integers-start-from n)
  (cons-stream n (integers-start-from (+ n 1))))
(define integers (integers-start-from 1))
