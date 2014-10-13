;;;; contlife.lisp

(in-package #:contlife)

(defun clamp (x min max)
  (if (< x min )
      min
      (if (> x max )
          max
          x)))

(defstruct transition
  (low 0.3)
  (high 0.75)
  (death -0.3)
  (life 0.05))

(defun trans (low high death life)
  (make-transition :low low :high high
                   :death death :life life))
  
(defun init-board (grid &key (probability 0.5))
  "Randomly set cells to t and nil."
  (loop for i from 0 below (array-dimension grid 0)
     do
       (setf cy 0)
       (loop for j from 0 below (array-dimension grid 1)
          do
            (if (> (/ (random 100.0) 100.0) probability )
                (setf (aref grid i j) (random 1.0))))))


(defun neighbor-weight (grid i j)
  "Count the neighbors of grid location i,j"
  (let ((ip (if (> 0 (- i 1)) (- (array-dimension grid 0) 1) (- i 1)))
        (jp (if (> 0 (- j 1)) (- (array-dimension grid 1) 1) (- j 1)))
        (in (if (>= i (- (array-dimension grid 0) 1)) 0 (+ i 1)))
        (jn (if (>= j (- (array-dimension grid 1) 1)) 0 (+ j 1)))
        (count (aref grid i j)))
    (incf count (aref grid ip jp))
    (incf count (aref grid ip j))
    (incf count (aref grid ip jn))
    (incf count (aref grid i jp))
    (incf count (aref grid i jn))
    (incf count (aref grid in jp))
    (incf count (aref grid in j))
    (incf count (aref grid in jn))
    count))

(defun update-board (old-grid new-grid &key (transition (make-transition)))
  "Update old-grid based on neighbor counts, placing the results in new-grid."
  (let ((low-life (transition-low transition))
        (high-life (transition-high transition))
        (death-amount (transition-death transition))
        (life-amount (transition-life transition)))
  (loop for i from 0 below (array-dimension old-grid 0)
     do
       (setf cy 0)
       (loop for j from 0 below (array-dimension old-grid 1)
          do
            (let ((nw (neighbor-weight old-grid i j)))
              (if (< low-life nw high-life)
                  (setf (aref new-grid i j) (clamp (+ (aref old-grid i j) life-amount) 0.0 1.0))
                  (setf (aref new-grid i j) (clamp (+ (aref old-grid i j) death-amount) 0.0 1.0))))))))

(defun draw-board (grid win-width win-height &key (multiplier 1.5))
  "Used OpenGL to display the grid."
  (gl:matrix-mode :modelview)
  (gl:push-matrix)
  (let ((dx (/ win-width (array-dimension grid 0)))
        (dy (/ win-height (array-dimension grid 1)))
        (cx 0)
        (cy 0))
    (gl:begin :quads)
    (loop for i from 0 below (array-dimension grid 0)
       do
         (setf cy 0)
         (loop for j from 0 below (array-dimension grid 1)
            do
              (let ((perc (aref grid i j)))
                (if (> perc 0.0)
                    (gl:color 0.0 (clamp (* multiplier perc) 0.0 1.0) 0.0)
                    (gl:color 0 0 0)))
              (progn 
                (gl:vertex cx cy)
                (gl:vertex (+ dx cx) cy)
                (gl:vertex (+ dx cx) (+ dy cy))
                (gl:vertex cx (+ dy cy)))
              (incf cy dy))
         (incf cx dx))
    (gl:end)
    (gl:pop-matrix)))

(defun start-life (&key (board-width 100) (board-height 100)
                     (win-width 800) (win-height 800)
                     (prob 0.5)
                     (multiplier 1.5)
                     (fps 30)
                     (delay 20)
                     (transition (make-transition)))
  "Run the game of life in an SDL window."


  (let
      ((boards (list
                (make-array `(,board-width ,board-height) :element-type 'real :initial-element 0.0)
                (make-array `(,board-width ,board-height) :element-type 'real :initial-element 0.0)))
       ;; boards is a cons cell pointing to two 2-d arrays of booleans
       (prev-tick 0) ;; prev-tick is the previous value of sdl-system-ticks when the board was updated
       (paused nil) ;; paused is t when paused, nil when not
       (trans (if (listp transition) (apply #'trans transition) transition)))
    (init-board (car boards) :probability prob )
    (sdl:with-init
        ()
      ;; Setup the window and view
      (sdl:window win-width win-height
                  :opengl t
                  :opengl-attributes '((:sdl-gl-depth-size   16)
                                       (:sdl-gl-doublebuffer 1)))
      (setf (sdl:frame-rate) fps)
      
      (gl:viewport 0 0 win-width win-height)
      (gl:matrix-mode :projection)
      (gl:load-identity)
      (gl:ortho 0.0 win-width 0.0 win-height -1.0 1.0)

      (gl:matrix-mode :modelview)
      (gl:load-identity)
      
      (gl:clear-color 0 0 0 0)
      (gl:shade-model :flat)
      (gl:cull-face :back)
      (gl:polygon-mode :front :fill)
      (gl:draw-buffer :back)
      (gl:enable :cull-face :depth-test)

      (gl:clear :color-buffer :depth-buffer)
      
      ;; Draw the initial board
      (draw-board (car boards) win-width win-height :multiplier multiplier)
      (sdl:update-display)

      ;; Handle events
      (sdl:with-events ()
        (:quit-event () t)
        
        (:key-down-event
         ()
         ;; quit
         (when (or (sdl:key-down-p :sdl-key-q) (sdl:key-down-p :sdl-key-escape))
           (sdl:push-quit-event))

         ;; Reset to a random state
         (when (sdl:key-down-p :sdl-key-r)
           (init-board (car boards)))

         ;; Pause/unpause
         (when (sdl:key-down-p :sdl-key-p)
           (if paused
               (setf paused nil)
               (setf paused t))))

        (:video-expose-event () (sdl:update-display))

        (:idle
         ;; Check if it's time to update
         (if (> (- (sdl:system-ticks) prev-tick) delay)
             (progn
               (setf prev-tick (sdl:system-ticks))

               ;; Only update the board while not paused
               (when (not paused)
                 (update-board (car boards) (cadr boards) :transition trans)
                 (setf boards (list (cadr boards) (car boards))))

               ;; Clear the screen and redraw
               (gl:clear :color-buffer :depth-buffer)
               (draw-board (car boards) win-width win-height :multiplier multiplier)
               (sdl:update-display))))))))

