#lang racket
(require graphics/graphics)
(open-graphics)
(define vp (open-viewport "A Picture Language" 500 500))

(define draw (draw-viewport vp))
(define (clear) ((clear-viewport vp)))
(define line (draw-line vp))
(define rectangel (draw-rectangle vp))
(define string (draw-string vp))

;combination functions
(define (add-posn posn1 posn2)
  (let ((res-x (+ (posn-x posn1) (posn-x posn2)))
        (res-y (+ (posn-y posn1) (posn-y posn2)))
        )
     (make-posn res-x res-y)))
(draw "white")
(define op (make-posn 10 20))
;(define p-offset (make-posn 20 30))
;(define p-rec (make-posn 30 40))
;(string (add-posn p-offset p-rec) "rec-name" "black")
;(rectangel p-rec 100 50)

(define (block posn info)
   (define p-offset (make-posn 20 30)) 
   (string (add-posn p-offset posn) info "black")
   (rectangel posn 100 50))
(define mp make-posn)
(define bk1-pos (mp 40 50))
(define bk2-pos (mp 100 200))
(block bk1-pos "ddd")

(block bk2-pos "eee")

;(define (connect block2 info block1)
  
(line bk1-pos bk2-pos "black")



