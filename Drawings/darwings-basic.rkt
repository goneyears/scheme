#lang racket
(require graphics/graphics)
(require (file "../basic/basic.rkt"))
;viewport basic operations
(open-graphics)
(define viewpoint (open-viewport "new block window" 500 500))

(define (display-posn posn)
  (display (posn-x posn))
  (display '-)
  (display (posn-y posn))
  (newline))

(define (setpoint viewpoint operate posn rgb)
  (cond ((= 0 (length rgb)) ((operate viewpoint) posn))
        ((= 1 (length rgb)) ((operate viewpoint) posn (car rgb)))
        (else (error "wrong rgb parameter"))))

(define (drawpoint viewpoint posn . rgb)
  (cond ((= 0 (length rgb)) (setpoint viewpoint draw-pixel posn))
        ((= 1 (length rgb)) (setpoint viewpoint draw-pixel posn (car rgb)))
        (else (error "wrong rgb parameter"))))

(define (clearpoint viewpoint posn)
  (setpoint viewpoint clear-pixel posn))




;square
(define (setsquare viewpoint operate posn a . rgb)
  (cond ((= 0 (length rgb)) ((operate viewpoint) (make-posn (- (posn-x posn) (/ 0 1)) (- (posn-y posn) (/ a 1))) a a))
        ((= 1 (length rgb)) ((operate viewpoint) (make-posn (- (posn-x posn) (/ 0 1)) (- (posn-y posn) (/ a 1))) a a (car rgb)))
        (else (error "wrong rgb parameter"))))

(define (drawsquare viewpoint p a . rgb)
    (cond ((= 0 (length rgb)) (setsquare viewpoint draw-rectangle p a))
          ((= 1 (length rgb)) (setsquare viewpoint draw-rectangle p a (car rgb)))
          (else (error "wrong rgb parameter"))))

(define (clearsquare viewpoint p a)
  (setsquare viewpoint clear-rectangle p a))

;cycle
(define (setcycle viewpoint operate posn r . rgb)
  (cond ((eq? rgb '()) ((operate viewpoint) (make-posn (- (posn-x posn) (/ r 2)) (- (posn-y posn) (/ r 2))) r r))
        ((= 1 (length rgb)) ((operate viewpoint) (make-posn (- (posn-x posn) (/ r 2)) (- (posn-y posn) (/ r 2))) r r (car rgb)))
        (else (error "wrong rgb parameter"))))

(define (drawcycle viewpoint p r . rgb)
    (cond ((= 0 (length rgb)) (setcycle viewpoint draw-ellipse p r))
          ((= 1 (length rgb)) (setcycle viewpoint draw-ellipse p r (car rgb)))
          (else (error "wrong rgb parameter"))))

(define (clearcycle viewpoint p r)
  (setsquare viewpoint clear-ellipse p r))

;(define (drawsolidcycle viewpoint p r . rgb)
;    (cond ((= 0 (length rgb)) (setcycle viewpoint draw-solid-ellipse p r))
;          ((= 1 (length rgb)) (setcycle viewpoint draw-solid-ellipse p r (car rgb)))
;          (else (error "wrong rgb parameter"))))

(define (drawsolidcycle viewpoint p r . rgb)
           (setcycle viewpoint draw-solid-ellipse p r (car rgb)))

(define (clearsolidcycle viewpoint p r)
  (setcycle viewpoint clear-solid-ellipse p r))

(drawsolidcycle viewpoint (make-posn 100 100) 100 )
;horizontal and vertical

(define (draw-horizontal viewpoint start end static)
  (define (draw-horizontal-int start end static)
    (cond ((= start end)
           (drawpoint viewpoint (make-posn start static)))
          ((< start end)
           (drawpoint viewpoint (make-posn start static))
           (draw-horizontal-int (+ start 1) end static))
          ((> start end)
           (drawpoint viewpoint (make-posn start static))
           (draw-horizontal-int (- start 1) end static))))
  (draw-horizontal-int (round start) (round end) (round static)))


(define (draw-vertical viewpoint start end static)
  (define (draw-vertical-int start end static)
    (cond ((= start end)
           (drawpoint viewpoint (make-posn static start)))
          ((< start end)
           (drawpoint viewpoint (make-posn static start))
           (draw-vertical-int (+ start 1) end static))
          ((> start end)
           (drawpoint viewpoint (make-posn static start))
           (draw-vertical-int (- start 1) end static))))
  (draw-vertical-int (round start) (round end) (round static)))

;line
(define (drawline viewpoint startposn endposn)
  (define (draw-line-start) (drawsquare viewpoint startposn 6))
  (define (draw-line-end) (drawsolidcycle viewpoint endposn 6))

  (draw-line-start)
  (draw-line-end)
  


  (define (draw-angle viewpoint startposn endposn)
    (let ((x1 (posn-x startposn))
          (y1 (posn-y startposn))
          (x2 (posn-x endposn))
          (y2 (posn-y endposn)))
      (draw-horizontal viewpoint x1 x2 y1)
      (draw-vertical viewpoint y1 y2 x2)))
 
  (define k
    (if (= (- (posn-x endposn) (posn-x startposn)) 0)
        'inf
        (/ (- (posn-y endposn) (posn-y startposn)) (- (posn-x endposn) (posn-x startposn)))))
  
  (displayln k)
  
  (define b
    (if (eq? k 'inf) 'null (- (posn-y endposn) (* k (posn-x endposn)))))
  
  (displayln b)
  
  (define (y x) (if (eq? k 'inf) 'null (round (+ (* k x) b))))

  (define (notexceed? startposn endposn)
    (cond ((eq? k 'inf)
           (not (= (posn-y startposn) (posn-y endposn))))
           (else (not (= (posn-x startposn) (posn-x endposn))))))

  (define crease (cond ((eq? k 'inf) (if (<= (posn-y startposn) (posn-y endposn)) 'increase 'decrease))
                         (else (if (<= (posn-x startposn) (posn-x endposn)) 'increase 'decrease))))
  
  (define (drawline-iter startposn endposn)
    (let ((s-x (posn-x startposn))
          (s-y (posn-y startposn))
          (e-x (posn-x endposn))
          (e-y (posn-y endposn)))
      (display (notexceed? startposn endposn))
      (cond ((notexceed? startposn endposn)
             (let ((nextposn
                     (if (eq? k 'inf)
                         (cond ((eq? crease 'increase) (make-posn s-x (+ s-y 1)))
                               ((eq? crease 'decrease) (make-posn s-x (- s-y 1))))
                         (cond ((eq? crease 'increase) (make-posn (+ s-x 1) (y (+ s-x 1))))
                               ((eq? crease 'decrease) (make-posn (- s-x 1) (y (- s-x 1))))))))
               (display-posn startposn)
               (display-posn nextposn)
               (draw-angle viewpoint startposn nextposn)
               (drawline-iter nextposn endposn))
             ))))
  
  (drawline-iter startposn endposn))

;(drawline viewpoint (make-posn 400 120) (make-posn 350 301))
;(drawsquare viewpoint (make-posn 390 130) 50)
  
;(drawsquare viewpoint (make-posn 330 320) 50)

