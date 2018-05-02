#lang racket
(define (reading)
  (let ((str (read in)))
    (cond ((eq? str 'ab) (display "A") )
          ((eq? str 'bc) (display "B"))
          (else (displayln str) (display (string-length str)))))
  (sleep 1)
  (displayln "reading")
  (reading))

(define the-listener (tcp-listen 9877 4 #f "127.0.0.1"))
(define-values (in out) (tcp-accept the-listener))

(file-stream-buffer-mode in 'none)
(reading)
(tcp-close the-listener)