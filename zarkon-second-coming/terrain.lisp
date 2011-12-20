
#-aichallenge (declaim (optimize (debug 3) (speed 0) (safety 3)))
#+aichallenge (declaim (optimize (debug 0) (speed 1) (safety 0)))

(defun inc-row-slow (row increment)
  (declare (optimize speed (safety 0))
           (type fixnum row increment))
  (incf row increment)
  (loop
     while (< row 0)
     do (incf row (the fixnum (rows *state*))))
  (loop
     while (>= row (the fixnum (rows *state*)))
     do (decf row (the fixnum (rows *state*))))
  row)

(defun fast-inc-row (row)
  (declare (optimize speed (safety 0))
           (type fixnum row))
  (let ((row+1 (1+ row)))
    (declare (type fixnum row+1))
    (if (>= row+1 *rows*)
        0
        row+1)))

(defun fast-dec-row (row)
  (declare (optimize speed (safety 0))
           (type fixnum row))
  (let ((row-1 (1- row)))
    (declare (type fixnum row-1))
    (if (< row-1 0)
        (1- (the (and fixnum (integer 1)) *rows*))
        row-1)))

(defmacro inc-row (row increment)
  (cond ((and (numberp increment)
              (= 1 increment))
         `(fast-inc-row ,row))
        ((and (numberp increment)
              (= -1 increment))
         `(fast-dec-row ,row))
        (t `(inc-row-slow ,row ,increment))))

(defun inc-col-slow (col increment)
  (declare (optimize speed (safety 0))
           (type fixnum col increment))
  (incf col increment)
  (loop
     while (< col 0)
     do (incf col (the fixnum (cols *state*))))
  (loop
     while (>= col (the fixnum (cols *state*)))
     do (decf col (the fixnum (cols *state*))))
  col)

(defun fast-inc-col (col)
  (declare (optimize speed (safety 0))
           (type fixnum col))
  (let ((col+1 (1+ col)))
    (declare (type fixnum col+1))
    (if (>= col+1 *cols*)
        0
        col+1)))

(defun fast-dec-col (col)
  (declare (optimize speed (safety 0))
           (type fixnum col))
  (let ((col-1 (1- col)))
    (declare (type fixnum col-1))
    (if (< col-1 0)
        (1- (the (and fixnum (integer 1)) *cols*))
        col-1)))

(defmacro inc-col (col increment)
  (cond ((and (numberp increment)
              (= 1 increment))
         `(fast-inc-col ,col))
        ((and (numberp increment)
              (= -1 increment))
         `(fast-dec-col ,col))
        (t `(inc-col-slow ,col ,increment))))

;; TODO needs better docstring (needs better code as well!)
(defun new-location (row col direction)
  (declare (optimize speed))
  "Returns '(NEW-ROW NEW-COL) for ROW,COL and DIRECTION for a grid that
  wraps around."
  (case direction
    ((:north) (values (inc-row row -1) col))
    ((:south) (values (inc-row row 1) col))
    ((:east) (values row (inc-col col 1)))
    ((:west) (values row (inc-col col -1)))
    (t (values row col))))

(defun distance (row1 col1 row2 col2)
  (declare (optimize speed (safety 0))
           (type fixnum row1 col1 row2 col2))
  "Returns the shortest distance between ROW1,COL1 and ROW2,COL2 for a grid
  that wraps around."
  (let* ((drow (abs (- row1 row2)))
         (dcol (abs (- col1 col2)))
         (minrow (min drow (the fixnum (- (the fixnum (rows *state*)) drow))))
         (mincol (min dcol (the fixnum (- (the fixnum (cols *state*)) dcol)))))
    (declare (type (and fixnum (integer 0 1000)) drow dcol minrow mincol))
    (+  (* minrow minrow) (*  mincol mincol))))

(defmacro mapmap-gen (row-incrementor col-incrementor)
  `(let ((ir start-row)
         (mr 0))
     (declare (type fixnum ir mr))
     (loop
        (let ((ic start-col)
              (mc 0))
          (declare (type fixnum ic mc))
          (loop
             (funcall fun ir ic mr mc)
             (when (= ic end-col)
               (return))
             (setf ic ,col-incrementor)
             (incf mc)))
        (when (= ir end-row)
          (return))
        (setf ir, row-incrementor)
        (incf mr))))

(defun mapmap-impl (start-row start-col end-row end-col fun)
  (declare (optimize speed (safety 0) (debug 3))
           (type fixnum start-row start-col end-row end-col)
           (type (function (fixnum fixnum fixnum fixnum) t) fun))
  (let ((fast-rows (< start-row end-row))
        (fast-cols (< start-col end-col)))
    (locally (declare (optimize (speed 2)))
      (cond ((and fast-rows fast-cols)
             (mapmap-gen (1+ ir) (1+ ic)))
            (fast-rows (mapmap-gen (1+ ir) (inc-col ic 1)))
            (fast-cols (mapmap-gen (inc-row ir 1) (1+ ic)))
            (t (mapmap-gen (inc-row ir 1) (inc-col ic 1)))))))

(defun mapmap (start-row start-col end-row end-col fun)
  (mapmap-impl start-row start-col end-row end-col
               (lambda (ir ic mr mc)
                 (declare (ignore mr mc))
                 (funcall fun ir ic))))

(defvar *distance-masks* (make-hash-table :test #'eql))

(defun get-distance-mask (distance distance2)
  (let ((result (gethash distance2 *distance-masks*)))
    (unless result
      (let* ((size (1+ (* 2 distance))))
        (setf result (make-array (list size size)
                                 :initial-element nil))
        (loop
           for ir from (- distance) to distance
           do (loop
                 for ic from (- distance) to distance
                 do (setf (aref result (+ ir distance) (+ ic distance))
                          (<= (+ (* ir ir) (* ic ic))
                              distance2))))
        (setf (gethash distance2 *distance-masks*) result)))
    result))

(defun mapmap-within-distance (row col distance distance2 fun)
  (let ((mask (get-distance-mask distance distance2)))
    (mapmap-impl (inc-row row (- distance))
                 (inc-col col (- distance))
                 (inc-row row distance)
                 (inc-col col distance)
                 (lambda (ir ic mr mc)
                   (when (aref mask mr mc)
                     (funcall fun ir ic))))))

(defmacro fast-operate-on (array element-type &body body)
  `(locally
       (declare (optimize speed))
     (let ((map ,array))
       (declare (type (simple-array ,element-type (200 200)) map))
       ,@body)))

(defun unknownp (row col)
  (fast-operate-on (terrain-map *state*) (mod 4)
    (= 0 (aref map row col))))

(defun landp (row col)
  (fast-operate-on (terrain-map *state*) (mod 4)
    (= 1 (aref map row col))))

(defun waterp (row col)
  (fast-operate-on (terrain-map *state*) (mod 4)
    (= 2 (aref map row col))))

(defun set-hill-standing-at (row col value)
  (let ((hill (aref (hills-map *state*) row col)))
    (when hill
      (setf (standing hill) value))))

(defun set-land-at (row col)
  (set-hill-standing-at row col nil)
  (fast-operate-on (terrain-map *state*) (mod 4)
    (setf (aref map row col) 1)))

(defun set-water-at (row col)
  (fast-operate-on (terrain-map *state*) (mod 4)
    (setf (aref map row col) 2)))

(defun set-food-at (row col)
  (fast-operate-on (food-map *state*) (mod 2)
    (setf (aref map row col) 1)))

(defun clear-fog-at (row col)
  (fast-operate-on (fog-map *state*) fixnum
    (setf (aref map row col) 0)))

(defun fogp (row col)
  (fast-operate-on (fog-map *state*) fixnum
    (> (aref map row col) 0)))

(defun fog-age (row col)
  (fast-operate-on (fog-map *state*) fixnum
    (aref map row col)))

(defun mark-unknown-as-land-at (row col)
  (declare (optimize speed))
  ;; (log-line "marking unknown as land")
  ;; (log-line "terrain is " (terrain-map *state*))
  (mapmap-within-distance row col (view-radius *state*) (view-radius2 *state*)
                          (lambda (ir ic)
                            (clear-fog-at ir ic)
                            ;; (log-line "at " ir " " ic " " (aref (terrain-map *state*) ir ic))
                            (when (unknownp ir ic)
                              (set-land-at ir ic)))))

(defun set-ant-at (row col owner)
  (let ((ant (make-instance 'ant
                            :row-now row
                            :col-now col
                            :next-row row
                            :next-col col
                            :owner owner)))
    (fast-operate-on (ants-map *state*) t
      (setf (aref map row col) ant))))

(defun get-ant-at (row col)
  (fast-operate-on (ants-map *state*) t
    (aref map row col)))

(defun set-hill-at (row col hill)
  (unless (aref (hills-map *state*) row col)
    (setf (aref (hills-map *state*) row col)
          hill)))

(defun foodp (row col)
  (fast-operate-on (food-map *state*) (mod 2)
    (= 1 (aref map row col))))

(defun accessiblep (row col)
  (declare (optimize speed))
  (and (landp row col)
       (not (foodp row col))))

(defun get-hill-at (row col)
  (aref (hills-map *state*) row col))

(defun unrazed-enemy-hill-at (row col)
  (aif (get-hill-at row col)
       (and (standing it)
            (not (= 0 (owner it))))))

(defun neighbors (row col fun)
  (declare (optimize speed)
           (type (function (fixnum fixnum) t) fun))
  (dolist (dir '(:north :east :south :west))
    (multiple-value-bind (nr nc)
        (new-location row col dir)
      (funcall fun nr nc))))

(defun unknown-neighbors (row col)
  (declare (optimize speed))
  (neighbors row col (lambda (r c)
                       (when (unknownp r c)
                         (return-from unknown-neighbors t))))
  nil)

(defun not-fog-neighbors (row col)
  (declare (optimize speed))
  (neighbors row col (lambda (r c)
                       (when (not (fogp r c))
                         (return-from not-fog-neighbors t))))
  nil)

(defun score-at (row col)
  (fast-operate-on *height-map* single-float
    (aref map row col)))

(defun forbidden-at (row col)
  (declare (optimize speed))
  (= -1s0 (the single-float (score-at row col))))

(defun forbid (row col)
  (fast-operate-on (hotspot-map *state*) single-float
    (setf (aref map row col) -1s0)))

(defun my-ants-neighbors (row col)
  (declare (optimize speed))
  (let (result)
    (neighbors row col (lambda (r c)
                         (awhen (get-ant-at r c)
                           (when (= 0 (the fixnum (owner it)))
                             (push it result)))))
    result))

(defun food-neighbors (row col)
  (declare (optimize speed))
  (neighbors row col (lambda (r c)
                       (when (foodp r c)
                         (return-from food-neighbors t))))
  nil)

(defun occupy-at (row col)
  ;; (log-line "occupy at " row " " col)
  (setf (aref (ant-occupied-map *state*) row col) t))

(defun unoccupy-at (row col)
  ;; (log-line "unoccupy at " row " " col)
  (setf (aref (ant-occupied-map *state*) row col) nil))

(defun occupied-at (row col)
  (aref (ant-occupied-map *state*) row col))
