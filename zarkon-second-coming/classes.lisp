
#-aichallenge (declaim (optimize (debug 3) (speed 0) (safety 3)))
#+aichallenge (declaim (optimize (debug 0) (speed 1) (safety 0)))

(defclass state ()
  ((rows :reader rows :initform nil)
   (cols :reader cols :initform nil)

   (terrain-map :accessor terrain-map :initform nil)
   (ants-map :accessor ants-map :initform nil)
   (hills-map :accessor hills-map :initform nil)
   (fog-map :accessor fog-map :initform nil)
   ;; (safe-map :accessor safe-map :initform nil)
   (food-map :accessor food-map :initform nil)
   (cluster-moves :accessor cluster-moves :initform nil)
   (rush-target :accessor rush-target :initform nil)
   (seen-food :accessor seen-food :initform nil)
   (lists-hash :accessor lists-hash :initform nil)

   (hill-distance-map :accessor hill-distance-map :initform nil)
   (battles-map :accessor battles-map :initform nil)
   (hotspot-map :accessor hotspot-map :initform nil)
   #-aichallenge (hmap-explain :accessor hmap-explain)
   (defensive-hotspot-map :accessor defensive-hotspot-map :initform nil)
   (hotspots :accessor hotspots :initform nil)
   (applied-hotspots :accessor applied-hotspots :initform nil)
   (food-list :accessor food-list :initform nil)
   (my-hills :accessor my-hills :initform nil)
   (all-my-hills :accessor all-my-hills :initform nil)
   (ants :accessor ants :initform nil)
   (ant-occupied-map :accessor ant-occupied-map :initform nil)
   (turn-time :reader turn-time :initform 1000)
   (load-time :reader load-time :initform 3000)
   (turn-start-time :reader turn-start-time :initform nil)
   (view-radius2 :reader view-radius2 :initform 93)
   (view-radius :reader view-radius :initform nil)
   (attack-radius2  :reader attack-radius2 :initform 6)
   (attack-radius  :reader attack-radius :initform nil)
   (spawn-radius2 :reader spawn-radius2 :initform 6)
   (turns :reader turns :initform nil)
   (turn :reader turn :initform nil)))

(let ((ant-counter 0))
  (defun next-ant-name ()
    (incf ant-counter)
    (format nil "ant-~a" ant-counter))
  (defun reset-ant-names ()
    (setf ant-counter 0)))

(defclass ant ()
  ((row-now :accessor row-now :initform nil :initarg :row-now)
   (col-now :accessor col-now :initform nil :initarg :col-now)
   (next-row :accessor next-row :initform nil :initarg :next-row)
   (next-col :accessor next-col :initform nil :initarg :next-col)
   (owner :accessor owner :initform 0 :initarg :owner)
   (order :accessor order :initform nil)
   (is-in-cluster :accessor is-in-cluster :initform nil)
   (is-processed :accessor is-processed :initform nil)))

(defclass hill ()
  ((row :accessor row :initform nil :initarg :row)
   (col :accessor col :initform nil :initarg :col)
   (standing :accessor standing :initarg :standing)
   (owner :accessor owner :initarg :owner)
   (to-protect :accessor to-protect :initform nil :initarg :to-protect)))

(defvar *state* (make-instance 'state))
(defvar *rows*)
(defvar *cols*)
(defvar *height-map*)

(declaim (type fixnum *rows* *cols*))

(defclass hotspot ()
  ((coords :accessor coords :initform nil :initarg :coords)
   (strength :accessor strength :initarg :strength)
   (kind :accessor kind :initarg :kind)
   (radius :accessor radius :initarg :radius :initform nil)
   (hs-color :accessor hs-color :initarg :hs-color :initform nil)
   (stop-at :accessor stop-at :initarg :stop-at :initform nil)
   (description :accessor description :initform nil :initarg :description)))

(defun strpot (hotspot)
  (cond (hotspot
         (format nil "hotspot ~s (strength ~a, description: \"~a\")~%coords: ~a"
                 (kind hotspot)
                 (strength hotspot)
                 (description hotspot)
                 (coords hotspot)))
        (t "nil hotspot")))

(let ((time-units (/ 1.0 internal-time-units-per-second)))
  ;; TODO correctly name function: doesn't return wall time
  ;; TODO use DOUBLE-FLOATs?
  (defun wall-time (&key (offset 0))
    "Returns the time in seconds (as a FLOAT) since SBCL was started."
    (+ (* (get-internal-real-time) time-units)
       offset)))

(defmacro turn-time-remaining ()
  "Returns the turn time remaining in seconds (as a FLOAT)."
  `(locally (declare (optimize speed (safety 0)))
     (- (+ (the single-float (turn-start-time *state*)) (the single-float (turn-time *state*)))
        (the single-float (wall-time)))))

(defun check-time ()
  (declare (optimize speed (safety 0)))
  (when (< (the single-float (turn-time-remaining)) (the single-float *cutoff-time*))
    (log-line "abort remaining ant processing during this term "
              (turn-time-remaining))
    (throw 'finish-turn nil)))

(defun soft-check-time ()
  (declare (optimize speed (safety 0)))
  (when (< (the single-float (turn-time-remaining)) (the single-float *cutoff-time*))
    (log-line "should abort remaining ant processing during this term "
              (turn-time-remaining))
    t))

(defun get-hashed (arg)
  (let ((hashed (gethash arg (lists-hash *state*))))
    (cond (hashed hashed)
          (t (setf (gethash arg (lists-hash *state*)) arg)
             arg))))
