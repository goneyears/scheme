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


(define (reverse lst)
  (if (null? lst)
      lst
      (cons (last lst) (reverse (pre-list lst (- (length lst) 1))))))

(define (even lst)
  (cond ((null? lst) '())
        ((= (length lst) 1) '())          
        (else (cons (cadr lst) (even (cddr lst))))))
(define (odd lst)
  (cond ((null? lst) '())
        ((= (length lst) 1) lst)
        (else (cons (car lst) (odd (cddr lst))))))
  
(define lst1 (list 1 2 3 4))
(define lst2 (list 5 6 7))
(define lst3 (append lst1 lst2))
(reverse lst3)
(even lst3)
(odd lst3)
(display '-------------)
(cons 3 4)