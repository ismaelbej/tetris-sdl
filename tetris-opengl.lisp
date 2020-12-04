;;;; tetris-opengl.lisp

(in-package #:tetris-opengl)

;; screen dimensions
(defparameter *width* 640)
(defparameter *height* 480)

(defparameter *cell-width* 20)
(defparameter *cell-height* 20)

;; board dimensions
(defparameter *board-width* 11)
(defparameter *board-height* 20)
(defparameter *board* nil)

;; colors 7 pices + void
(defparameter *colors* (make-array '8))

(defparameter *rgb-colors* '((0 0 0)        ;; 0 - black
                             (170 0 0)      ;; 1 - maroon
                             (255 255 255)  ;; 2 - white
                             (170 0 170)    ;; 3 - magenta
                             (0 0 170)      ;; 4 - dark blue
                             (0 170 0)      ;; 5 - green
                             (170 85 0)     ;; 6 - brown
                             (0 170 170)))  ;; 7 - cyan

(defparameter *tetrominoes* '(((1 1 1 1)) ;; I
                              ((2 2 2)    ;; J
                               (0 0 2))
                              ((3 3 3)    ;; L
                               (3 0 0))
                              ((4 4)      ;; O
                               (4 4))
                              ((0 5 5)    ;; S
                               (5 5 0))
                              ((6 6 6)    ;; T
                               (0 6 0))
                              ((7 7 0)    ;; Z
                               (0 7 7))))

(defparameter *pentominoes* '(((1 1 1 1 1))
                              ((0 2 2)
                               (2 2 0)
                               (0 2 0))
                              ((3 3 0)
                               (0 3 3)
                               (0 3 0))
                              ((4 4 4 4)
                               (0 0 0 4))
                              ((5 5 5 5)
                               (5 0 0 0))
                              ((6 6 6)
                               (6 6 0))
                              ((7 7 7)
                               (0 7 7))
                              ((1 1 0 0)
                               (0 1 1 1))
                              ((0 0 2 2)
                               (2 2 2 0))
                              ((3 3 3)
                               (0 3 0)
                               (0 3 0))
                              ((4 0 4)
                               (4 4 4))
                              ((0 0 5)
                               (0 0 5)
                               (5 5 5))
                              ((0 0 6)
                               (0 6 6)
                               (6 6 0))
                              ((0 7 0)
                               (7 7 7)
                               (0 7 0))
                              ((1 1 1 1)
                               (0 1 0 0))
                              ((2 2 2 2)
                               (0 0 2 0))
                              ((3 0 0)
                               (3 3 3)
                               (0 0 3))
                              ((0 0 4)
                               (4 4 4)
                               (4 0 0))))
                            
(defparameter *pieces* *tetrominoes*)

(defparameter *piece-x* 0)
(defparameter *piece-y* 0)
(defparameter *piece-width* 0)
(defparameter *piece-height* 0)
(defparameter *piece* nil)

(defparameter *fps* 60)
(defparameter *ticks* 0)
(defparameter *level* 0)
(defparameter *lines* 0)
(defparameter *score* 0)

(defun get-sdl-color (c)
  (aref *colors* c))

(defun board-border-draw ()
  (let ((xp (+ (/ (- *width* (* *board-width* *cell-width*)) 2) -5))
        (yp 0)
        (width (+ (* *board-width* *cell-width*) 10))
        (height (+ (* *board-height* *cell-height*) 10))
        (clr (sdl:color :r 12 :g 12 :b 12)))
    (sdl:with-color (clr)
      (sdl:draw-box
       (sdl:rectangle :x xp :y yp :w width :h height)))))

(defun board-cell-draw (x y c)
  (when (> c 0)
    (let ((xpos (+ (* x *cell-width*) (/ (- *width* (* *board-width* *cell-width*)) 2)))
          (ypos (+ 5 (* y *cell-height*)))
          (clr (get-sdl-color c)))
      (sdl:with-color (clr)
        (sdl:draw-box
         (sdl:rectangle :x xpos :y ypos 
                        :w (1- *cell-width*) 
                        :h (1- *cell-height*)))))))

(defun board-row-draw (row y)
  (loop for x from 0 for cell in row do
       (board-cell-draw x y cell)))

(defun board-draw (board)
  (loop for y from 0 for row in board do
       (board-row-draw row y)))

(defun piece-row-draw (row y xp yp)
  (loop for x from 0 for cell in row do
       (board-cell-draw (+ xp x) (+ yp y) cell)))

(defun piece-draw (piece xp yp)
  (loop for y from 0 for row in piece do
       (piece-row-draw row y xp yp)))

(defun piece-check-row-pos (brow row xp)
  (loop for x from 0 for cell in row
     for bcell = (nth xp brow) then (nth (+ xp x) brow) always
       (or (zerop cell)
           (zerop bcell))))

(defun piece-check-pos-size (width height xp yp)
  (and (>= xp 0)
       (>= yp 0)
       (<= (+ xp width) *board-width*)
       (<= (+ yp height) *board-height*)))

(defun piece-check-pos (piece xp yp)
  (let ((height (length piece))
        (width (length (car piece))))
    (and (piece-check-pos-size width height xp yp)
         (loop for y from 0 for row in piece
            for brow = (nth yp *board*) then (nth (+ yp y) *board*) always
              (piece-check-row-pos brow row xp)))))

(defun score-draw (score lines level xp yp)
  (let ((score-text (format nil "~a" score))
        (level-text (format nil "~a" level))
        (lines-text (format nil "~a" lines)))
    (sdl:with-color (sdl:*white*)
      (sdl:with-default-font (sdl:*default-font*)
        (sdl:draw-string-solid-* "SCORE" xp yp)
        (sdl:draw-string-solid-* score-text xp (+ yp 20))
        (sdl:draw-string-solid-* "LINES" xp (+ yp 40))
        (sdl:draw-string-solid-* lines-text xp (+ yp 60))
        (sdl:draw-string-solid-* "LEVEL" xp (+ yp 80))
        (sdl:draw-string-solid-* level-text xp (+ yp 100))))))

(defun draw ()
  (sdl:clear-display (sdl:color :r 128 :g 128 :b 128))
  (score-draw *score* *lines* *level* 20 20)
  (board-border-draw)
  (board-draw *board*)
  (piece-draw *piece* *piece-x* *piece-y*)
  (sdl:update-display))

(defun board-fill (width height f)
  (loop for y from 0 below height collect
       (loop for x from 0 below width collect (funcall f x y))))

(defun board-init ()
  (setf *board* (board-fill *board-width* *board-height*
                            (lambda (x y) 0))))

(defun colors-init ()
  (loop for i from 0 for (r g b) in *rgb-colors* do
       (setf (aref *colors* i)
             (sdl:color :r r :g g :b b))))

(defun piece-set (piece piece-x piece-y)
  (setf *piece* piece)
  (setf *piece-width* (length *piece*))
  (setf *piece-height* (length (car *piece*)))
  (setf *piece-x* piece-x)
  (setf *piece-y* piece-y))
  

(defun piece-init ()
  (let* ((piece (nth (random (length *pieces*)) *pieces*))
         (piece-height (length piece))
         (piece-width (length (car piece)))
         (piece-x (floor (/ (- *board-width* piece-width) 2)))
         (piece-y 0))
    (loop
       (when (piece-check-pos piece piece-x piece-y)
         (piece-set piece piece-x piece-y)
         (return))
       (when (>= piece-y piece-height)
         (setf *piece* nil)
         (return))
       (incf piece-y))))
  
(defun init-data ()
  (sdl:initialise-default-font)
  (setf *score* 0)
  (setf *level* 0)
  (setf *lines* 0)
  (colors-init)
  (board-init)
  (piece-init))

(defun piece-rotate-left (piece)
  (reduce (lambda (x y) (mapcar (lambda (z w) (cons w z)) x y))
   piece :initial-value (make-sequence 'list (length (car piece)))))

(defun piece-copy (board piece xp yp)
  (loop for y from 0 for row in piece do
       (loop for x from 0 for cell in row do
            (when (> cell 0)
              (setf (nth (+ xp x) (nth (+ yp y) board)) cell)))))

(defun line-complete (row)
  (loop for cell in row always (not (zerop cell))))

(defun board-remove-complete-lines (board)
  (loop for row in board when (not (line-complete row)) collect row))

(defun calc-score (lines)
  (cond
    ((<= lines 0) 0)
    ((= lines 1) 100)
    ((= lines 2) 300)
    ((= lines 3) 700)
    ((>= lines 4) 1600)))

(defun board-clear-lines (board)
  (let* ((board (board-remove-complete-lines board))
         (lines (- *board-height* (length board)))
         (score (calc-score lines)))
    (incf *score* score)
    (incf *lines* lines)
    (setf *level* (floor (/ *lines* 10)))
    (loop repeat lines do
         (setf board (cons (make-sequence 'list *board-width* :initial-element 0) board)))
    board))

(defun update-new-piece ()
  (piece-copy *board* *piece* *piece-x* *piece-y*)
  (setf *board* (board-clear-lines *board*))
  (piece-init))

(defun update-piece ()
  (when (not (null *piece*))
    (if (piece-check-pos *piece* *piece-x* (1+ *piece-y*))
        (incf *piece-y*)
        (update-new-piece))))

(defun update ()
  (decf *ticks*)
  (when (<= *ticks* 0)
    (update-piece)
    (setf *ticks* (floor (* *fps* (expt 0.8 *level*))))))

(defun rotate-piece-left ()
  (let ((piece (piece-rotate-left *piece*))
        (piece-x *piece-x*)
        (piece-y *piece-y*))
    (when (piece-check-pos piece piece-x piece-y)
      (piece-set piece piece-x piece-y))))

(defun move-piece (piece piece-x piece-y)
  (when (piece-check-pos piece piece-x piece-y)
    (piece-set piece piece-x piece-y)))

(defun move-piece-left ()
  (move-piece *piece* (1- *piece-x*) *piece-y*))

(defun move-piece-right ()
  (move-piece *piece* (1+ *piece-x*) *piece-y*))

(defun move-piece-down ()
  (move-piece *piece* *piece-x* (1+ *piece-y*)))

(defun process-key (key)
  (cond
    ((sdl:key= key :sdl-key-escape)
     (sdl:push-quit-event))
    ((sdl:key= key :sdl-key-left)
     (move-piece-left))
    ((sdl:key= key :sdl-key-right)
     (move-piece-right))
    ((sdl:key= key :sdl-key-up)
     (rotate-piece-left))
    ((sdl:key= key :sdl-key-down)
     (move-piece-down))))

(defun start-game ()
  (init-data)
  (sdl:with-init ()
    (sdl:window *width* *height*
                :title-caption "Tetris"
                :fps (make-instance 'sdl:fps-fixed))
    (setf (sdl:frame-rate) *fps*)
    (sdl:enable-key-repeat nil nil)
    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event (:key key)
                       (process-key key))
      (:idle ()
             (update)
             (draw)))))

(defun play ()
  (start-game))
  