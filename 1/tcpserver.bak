#lang racket
(define (reading)
  (displayln (read in))
  (sleep 1)
  (displayln "reading")
  (reading))

(define the-listener (tcp-listen 9876))
(define-values (in out) (tcp-accept the-listener))
(reading)
(tcp-close the-listener)