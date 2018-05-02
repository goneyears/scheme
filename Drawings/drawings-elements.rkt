#lang racket
(require graphics/graphics)

;viewport basic operations
(open-graphics)
(define vp (open-viewport "new block window" 500 500))

;basic operations
(define (half val)
  (/ val 2))
(define (third val)
  (/ val 3))
(define (add-posn pos1 pos2)
  (make-posn (+ (posn-x pos1) (posn-x pos2))
             (+ (posn-y pos1) (posn-y pos2))))

(define (negative posn)
  (make-posn (- (posn-x posn)) (- (posn-y posn))))

(define (sub-posn pos1 pos2)
  (add-posn pos1 (negative pos2)))

(define (posn-offset posn offset-x offset-y)
  (make-posn (+ (posn-x posn) offset-x) (+ (posn-y posn) offset-y)))

;drawline new definition
(define (drawpoint posn)
   ;(sleep 0.00001)
   ((draw-pixel vp) posn))
(define (displays . messages)
  (define (display-iter messagelists)
    (cond ((> (length messagelists) 0)
              (display (car messagelists))
              (newline)
              (display-iter (cdr messagelists)))))
  (display-iter messages)
  )

(define (display-posn posn)
  (display (posn-x posn))
  (display '-)
  (display (posn-y posn))
  (newline))

(define (drawsquare p a)
  ((draw-rectangle vp) (make-posn (- (posn-x p) (/ 0 1)) (- (posn-y p) (/ a 1))) a a))

(define (drawcycle p r)
  ((draw-ellipse vp) (make-posn (- (posn-x p) (/ r 2)) (- (posn-y p) (/ r 2))) r r))

(define (drawsolidcycle p r)
  ((draw-solid-ellipse vp) (make-posn (- (posn-x p) (/ r 2)) (- (posn-y p) (/ r 2))) r r))

(define (drawline startposn endposn)
  (define (draw-line-start) (drawsquare startposn 6))
  (define (draw-line-end) (drawsolidcycle endposn 6))

  (draw-line-start)
  (draw-line-end)
  
  (define (draw-horizontal start end static)
    (define (draw-horizontal-int start end static)
      (cond ((= start end)
             (drawpoint (make-posn start static)))
            ((< start end)
             (drawpoint (make-posn start static))
             (draw-horizontal-int (+ start 1) end static))
            ((> start end)
             (drawpoint (make-posn start static))
             (draw-horizontal-int (- start 1) end static))))
    (draw-horizontal-int (round start) (round end) (round static)))


  (define (draw-vertical start end static)
    (define (draw-vertical-int start end static)
      (cond ((= start end)
             (drawpoint (make-posn static start)))
            ((< start end)
             (drawpoint (make-posn static start))
             (draw-vertical-int (+ start 1) end static))
            ((> start end)
             (drawpoint (make-posn static start))
             (draw-vertical-int (- start 1) end static))))
    (draw-vertical-int (round start) (round end) (round static)))

  (define (draw-angle startposn endposn)
    (let ((x1 (posn-x startposn))
          (y1 (posn-y startposn))
          (x2 (posn-x endposn))
          (y2 (posn-y endposn)))
      (draw-horizontal x1 x2 y1)
      (draw-vertical y1 y2 x2)))
 
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
               (draw-angle startposn nextposn)
               (drawline-iter nextposn endposn))
             ))))
  
  (drawline-iter startposn endposn))


;basic elements
  ;line
(define (line start-posn end-posn)
  (define (dispatch m)
    (cond ((eq? m 'start-posn) start-posn)
          ((eq? m 'end-posn) end-posn)
          ((eq? m 'draw)
           (drawline start-posn end-posn))))
  dispatch)
(define (display-line lne) (lne 'draw))
(define display-string (draw-string vp))
  ;rectangle
(define (rectangle posn width height)
   (define (dispatch m)
     (let ((upper-left posn)
           (upper-right (posn-offset posn width 0))
           (bottom-left (posn-offset posn 0 height))
           (bottom-right (posn-offset posn width height))
           (upper-center (posn-offset posn (/ width 2) 0))
           (bottom-center (posn-offset posn (/ width 2) height))
           (center (posn-offset posn (/ width 2) (/ height 2)))
           )      
       (cond ((eq? m 'posn) posn)
             ((eq? m 'width) width)
             ((eq? m 'height) height)
             ((eq? m 'upper-left) upper-left)
             ((eq? m 'upper-right) upper-right)
             ((eq? m 'bottom-left) bottom-left)
             ((eq? m 'bottom-right) bottom-right)
             ((eq? m 'upper-center) upper-center)
             ((eq? m 'bottom-center) bottom-center)
             ((eq? m 'center) center)
             ((eq? m 'draw)
              ((draw-rectangle vp) posn width height))
             (else (error "Argument should be posn/width/height")))))
    dispatch)
(define (position-rectangle rec) (rec 'posn))
(define (width-rectangle rec) (rec 'width))
(define (height-rectangle rec) (rec 'height))
(define (display-rectangle rec) (rec 'draw))
 
;advanced basic element
(define (block posn width height info)
  (define block-frame-rectangle (rectangle posn width height))
  (define (dispatch m)
    (let ((center-posn (posn-offset posn (third width) (half height))))
      (cond ((eq? m 'draw)
             (begin
               (display-rectangle block-frame-rectangle)
               (display-string center-posn info)
               ))
            (else (block-frame-rectangle m)))))
  dispatch)
(define (display-block blk) (blk 'draw))
(define (position-block blk) (blk 'posn))
(define (width-block blk) (blk 'width))
(define (height-block blk) (blk 'height))
(define (upper-left-block blk) (blk 'upper-left))
(define (upper-right-block blk) (blk 'upper-right))
(define (bottom-left-block blk) (blk 'bottom-left))
(define (bottom-right-block blk) (blk 'bottom-right))
(define (upper-center-block blk) (blk 'upper-center))
(define (bottom-center-block blk) (blk 'bottom-center))
(define (center-block blk) (blk 'center))

;advanced elements operations
;(define (connector block1 block2)
;  (line (bottom-center-block block1) (upper-center-block block2)))

(define (connector block1 block2)
  (line (center-block block1) (center-block block2)))
(define (connect block1 block2)
  (sleep 0.2)
  (display-line (connector block1 block2)))

;convenient elements
(define (program-block info pos-x pos-y)
  (block (make-posn pos-x pos-y) 100 50 info))

(define (add-block info)
  (program-block info (posn-x (mouse-posn)) (posn-y (mouse-posn))))
;convenient functions
(define (mouse-posn) (query-mouse-posn vp))
(define (show-mouse-posn)
  (display (posn-x (mouse-posn)))
  (newline)
  (display (posn-y (mouse-posn))))


;(define block1 (block (make-posn 50 66) 100 50 "main"))
(define blk1 (program-block "blk1" 50 66))
(display-block blk1)

;(define block2 (block (make-posn 100 133) 100 50 "agv"))
(define blk2 (program-block "blk2" 100 133))
(display-block blk2)

;(define block3 (block (make-posn 100 233) 100 50 "sql"))
(define blk3 (program-block "blk3" 100 233))
(display-block blk3)

(define test (program-block "test" 200 33))
(display-block test)

(define test1 (program-block "test1" 300 133))
(display-block test1)

(define test2 (program-block "test2" 300 233))
(display-block test2)

  
