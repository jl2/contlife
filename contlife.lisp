;;;; contlife.lisp

(in-package #:contlife)

(defun clamp (x min max)
  (if (< x min )
      min
      (if (> x max )
          max
          x)))

(defstruct transition
  (low -0.75)
  (high 0.75)
  (death (/ -6.0 255))
  (life (/ 18.0 255)))

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
            ;; (setf (aref grid i j) (- 1.0 (random 2.0))))))
            (if (> (random 1.0) probability )
                (setf (aref grid i j) (- 1.0 (random 2.0)))
                (setf (aref grid i j) 0.0)))))

(defun neighbor-weight (grid i j)
  "Count the neighbors of grid location i,j"
  (let ((ip (if (> 0 (- i 1)) (- (array-dimension grid 0) 1) (- i 1)))
        (jp (if (> 0 (- j 1)) (- (array-dimension grid 1) 1) (- j 1)))
        (in (if (>= i (- (array-dimension grid 0) 1)) 0 (+ i 1)))
        (jn (if (>= j (- (array-dimension grid 1) 1)) 0 (+ j 1)))

        (ip2 (if (> 0 (- i 2)) (- (array-dimension grid 0) 2) (- i 2)))
        (jp2 (if (> 0 (- j 2)) (- (array-dimension grid 1) 2) (- j 2)))
        (in2 (if (>= i (- (array-dimension grid 0) 2)) 0 (+ i 2)))
        (jn2 (if (>= j (- (array-dimension grid 1) 2)) 0 (+ j 2)))
        
        (count 0.0))

    (incf count (* 1.0 (aref grid ip2 jp2)))
    (incf count (* -1.0 (aref grid ip2 jp)))
    (incf count (* -1.0 (aref grid ip2 j)))
    (incf count (* -1.0 (aref grid ip2 jn)))
    (incf count (* 1.0 (aref grid ip2 jn2)))

    (incf count (* -1.0 (aref grid ip jp2)))
    (incf count (* 2.0 (aref grid ip jp)))
    (incf count (* 2.0 (aref grid ip j)))
    (incf count (* 2.0 (aref grid ip jn)))
    (incf count (* -1.0 (aref grid ip jn2)))

    (incf count (* -1.0 (aref grid i jp2)))
    (incf count (* 2.0 (aref grid i jp)))
    (incf count (* 3.0 (aref grid i j)))
    (incf count (* 2.0 (aref grid i jn)))
    (incf count (* -1.0 (aref grid i jn2)))

    (incf count (* -1.0 (aref grid in jp2)))
    (incf count (* 2.0 (aref grid in jp)))
    (incf count (* 2.0 (aref grid in j)))
    (incf count (* 2.0 (aref grid in jn)))
    (incf count (* -1.0 (aref grid in jn2)))

    (incf count (* 1.0 (aref grid in2 jp2)))
    (incf count (* -1.0 (aref grid in2 jp)))
    (incf count (* -1.0 (aref grid in2 j)))
    (incf count (* -1.0 (aref grid in2 jn)))
    (incf count (* 1.0 (aref grid in2 jn2)))

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
                  (setf (aref new-grid i j) (clamp (+ (aref old-grid i j) life-amount) -1.0 1.0))
                  (setf (aref new-grid i j) (clamp (+ (aref old-grid i j) death-amount) -1.0 1.0))))))))

(defun draw-board (grid win-width win-height &key (multiplier 1.5))
  "Used OpenGL to display the grid."
  (gl:matrix-mode :modelview)
  (gl:push-matrix)
  (let ((dx (/ win-width (array-dimension grid 0)))
        (dy (/ win-height (array-dimension grid 1)))
        (cx 0)
        (cy 0))
    (gl:begin :points)
    (loop for i from 0 below (array-dimension grid 0)
       do
         (setf cy 0)
         (loop for j from 0 below (array-dimension grid 1)
            do
              (let ((perc (aref grid i j)))
                (if (> perc 0.0)
                    (gl:color 0.0 (clamp (* multiplier perc) 0.0 1.0) 0.0)
                    (gl:color (clamp (* -1.0 multiplier perc) 0.0 1.0) 0.0 0.0))
                (gl:vertex cx cy))
              (incf cy dy))
         (incf cx dx))
    (gl:end)
    (gl:pop-matrix)))



(defun handle-window-size (win-width win-height board-width board-height)
  "Adjusting the viewport and projection matrix for when the window size changes."

  (gl:viewport 0 0 win-width win-height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho 0.0 win-width 0.0 win-height -1.0 1.0)
  (gl:clear :color-buffer :depth-buffer)
  (gl:point-size (/ win-width board-width 0.9)))

(defun start-life (&key (board-width 200) (board-height 200)
                     (prob 0.5)
                     (multiplier 1.01)
                     (fps 60)
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
    (sdl2:with-init (:everything)
      (sdl2:with-window (window :title "filevis"
                                :flags '(:shown :resizable :opengl))
        (sdl2:with-gl-context (gl-context window)

          (sdl2:gl-make-current window gl-context)

          (multiple-value-bind (win-width win-height) (sdl2:get-window-size window)
            (handle-window-size win-width win-height board-width board-height)

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
            (gl:flush)
            (sdl2:gl-swap-window window)
            (sdl2:with-event-loop (:method :poll :timeout 1/20)
              (:windowevent
               (:event event :data1 w :data2 h)
               (when (= event sdl2-ffi:+sdl-windowevent-resized+)
                 (setf win-width w)
                 (setf win-height h)
                 (handle-window-size w h board-width board-height)))
              

              (:keyup
               (:keysym keysym)
               (when (or (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                         (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-q))
                 (sdl2:push-event :quit))
               (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-r)
                 (init-board (car boards) :probability prob))

               (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-p)
                 (if paused
                     (setf paused nil)
                     (setf paused t))))


              (:idle
                ()
                ;; Check if it's time to update
                (setf prev-tick (sdl2:get-ticks))

                ;; Only update the board while not paused
                (when (not paused)
                  (update-board (car boards) (cadr boards) :transition trans)
                  (setf boards (list (cadr boards) (car boards))))

                ;; Clear the screen and redraw
                (gl:clear :color-buffer :depth-buffer)
                (draw-board (car boards) win-width win-height :multiplier multiplier)
                (gl:flush)
                (sdl2:gl-swap-window window)
                (sleep (/ 1.0 fps)))
              (:quit () t))))))))



(defun main (args)
  (declare (ignorable args))
  (sdl2:make-this-thread-main 
   (lambda ()
     (time (start-life)))))
