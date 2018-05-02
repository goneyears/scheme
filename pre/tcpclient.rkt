#lang racket
(define (send-string str)
  (write str out)
  (flush-output out))

(define-values (in out) (tcp-connect "localhost" 9876))
(write "Hello111" out)

(flush-output out)
(write "Hello222" out)
(flush-output out)
;(close-input-port in)
;(close-output-port out)
(display "end")