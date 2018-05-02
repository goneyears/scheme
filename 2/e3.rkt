#lang racket
(define (list-ref lst n)
  (cond ((null? lst) '())
        ((= n 0) (car lst))
        (else (list-ref (cdr lst) (- n 1)))))
      
(define (length lst)
  (if (null? lst)
      0
      (+ 1 (length (cdr lst)))))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (last lst)
  (list-ref lst (- (length lst) 1)))

(define (pre-list olist n)
  (define (iter lst i)
    (if (= i n) lst
        (iter (append lst (list (list-ref olist i))) (+ i 1))))
  (cond ((= n 0) '())
        (else (iter (list (car olist)) 1))))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (no-more? lst)
  (null? lst))

(define (first-denomination coins-list)
  (car coins-list))

(define (except-first-denomination coins-list)
 (cdr coins-list))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(cc 100 us-coins)


