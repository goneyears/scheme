#lang racket
(require graphics/graphics)
(require r5rs )
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation --TABLE" m))))
    dispatch))

;interface
(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(put 'aa 'bb 1)
(get 'aa 'bb)
(get 'aa 'cc)
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

;basic elements
  ;line
(define (line start-posn end-posn)
  (define (dispatch m)
    (cond ((eq? m 'start-posn) start-posn)
          ((eq? m 'end-posn) end-posn)
          ((eq? m 'draw)
           ((draw-line vp) start-posn end-posn))))
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

;advanced elements operations
(define (connector block1 block2)
  (line (bottom-center-block block1) (upper-center-block block2)))
(define (connect block1 block2)
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
(define main-prg (program-block "main" 50 66))
(display-block main-prg)

;(define block2 (block (make-posn 100 133) 100 50 "agv"))
(define agv-prg (program-block "agv" 100 133))
(display-block agv-prg)

;(define block3 (block (make-posn 100 233) 100 50 "sql"))
(define sql-prg (program-block "sql" 100 233))
(display-block sql-prg)
(connect main-prg agv-prg)
(connect agv-prg sql-prg)

