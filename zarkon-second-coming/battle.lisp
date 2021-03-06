
#-aichallenge (declaim (optimize (debug 3) (speed 0) (safety 3)))
#+aichallenge (declaim (optimize (debug 0) (speed 1) (safety 0)))

(defvar *test*)

(setf *test* '(((100 103)
                (nil ((100 100) :east))
                (:north ((100 100) :east))
                (:south ((100 100) :east) ((102 100) :east))
                (:west ((100 100) nil :east :south) ((102 100) :north :east)))
               ((101 103)
                (nil ((100 100) :east) ((102 100) :east))
                (:north ((100 100) :east))
                (:south ((102 100) :east))
                (:west ((100 100) nil :east :south) ((102 100) nil :east :north)))
               ((102 104)
                (:west ((102 100) :east)))
               ((98 103)
                (:south ((100 100) :east))
                (:west ((100 100) :north)))))

(defvar *rev-test*)

(setf *rev-test* '(((100 100)
                    (:east
                     ((100 103) nil :north :south :west)
                     ((101 103) nil :north :west) ((98 103) :south))
                    (nil ((100 103) :west) ((101 103) :west))
                    (:north ((100 103) :west) ((98 103) :west))
                    (:south ((100 103) :west) ((101 103) :west)))
                   ((102 100)
                    (:east ((100 103) :south) ((101 103) nil :south :west) ((102 104) :west))
                    (:north ((101 103) :west)))))

(defvar *pos*)

(setf *pos* '(((100 103) nil)
              ((101 103) nil)
              ((102 104) :west)
              ((98 103) :south)
              ((100 100) :east)
              ((102 100) :east)))

(defvar *pos-us*)
(defvar *pos-them*)

(setf *pos-us* '(((100 103) nil)
                 ((101 103) nil)
                 ((102 104) :west)
                 ((98 103) :south)))
(setf *pos-them* '(((100 100) :east)
                   ((102 100) :east)))

(defun fast-member (p1 p2 list)
  (cond ((null list) nil)
        ((and (eql p1 (caar list))
              (eql p2 (cadar list)))
         t)
        (t (fast-member p1 p2 (rest list)))))

(defun get-ant-enemies (ant map)
  (declare (optimize speed))
  (bif (ant-record (first (member (first ant) map
                                  :test #'eql :key #'first)))
       (bif (ant-direction (first (member (second ant) (rest ant-record)
                                          :test #'eql :key #'first)))
            (rest ant-direction))))

(defun eval-focus (ant pos1 pos2 the-map)
  (declare (optimize speed (safety 0)))
  ;; (log-line "evaluate focus of ant " ant)
  (let ((result 0))
    (declare (type fixnum result))
    (labels ((handle-enemy (enemy)
               (dolist (enemy-dir (rest enemy))
                 (when (or (fast-member (first enemy) enemy-dir pos1)
                           (fast-member (first enemy) enemy-dir pos2))
                   ;; (log-line "sees " (list (first enemy) enemy-dir))
                   (incf result)))))
      (dolist (enemy (get-ant-enemies ant the-map))
        (handle-enemy enemy)))
    ;; (log-line "focus is " result)
    result))

(defun ant-dies-p (ant focus focus-hash map pos1 pos2)
  (declare (optimize speed))
  ;; (log-line "ant eval for " ant)
  (when (>= (the fixnum focus) 0)
    (dolist (enemy (get-ant-enemies ant map))
      (dolist (enemy-dir (rest enemy))
        (when (or (fast-member (first enemy) enemy-dir pos1)
                  (fast-member (first enemy) enemy-dir pos2))
          ;; (log-line "sees " (list (first enemy) enemy-dir))
          (let ((enemy-focus (gethash (get-hashed (list (first enemy) enemy-dir)) focus-hash)))
            (when (<= (the fixnum enemy-focus) (the fixnum focus))
              (return-from ant-dies-p t))))))))

(defun eval-position (pos-us pos-them the-map)
  (declare (optimize speed (safety 0)))
  (let ((focus-hash (make-hash-table :test #'eql)))
    (dolist (ant pos-them)
      ;; (log-line "focus for " ant " " (eval-focus ant pos-us pos-them the-map))
      (setf (gethash ant focus-hash) (eval-focus ant pos-us pos-them the-map)))
    (dolist (ant pos-us)
      ;; (log-line "focus for " ant " " (eval-focus ant pos-them nil the-map))
      (setf (gethash ant focus-hash) (eval-focus ant pos-them nil the-map)))
    (let ((our-losses 0)
          (their-losses 0))
      (declare (type fixnum our-losses their-losses))
      (dolist (ant pos-them)
        (let ((focus (gethash ant focus-hash)))
          (when (ant-dies-p ant focus focus-hash the-map pos-us pos-them)
            (incf their-losses))))
      (dolist (ant pos-us)
        (let ((focus (gethash ant focus-hash)))
          (when (ant-dies-p ant focus focus-hash the-map pos-them nil)
            (incf our-losses))))
      (values our-losses their-losses))))

(defun gen-positions (map occupied force-nil-direction nil-first for-us cb &optional so-far)
  (declare (optimize speed)
           (type (function (t) t) cb)
           (type (simple-array (mod 2) (200 200)) occupied))
  (cond ((null map) (funcall cb so-far))
        (t (let* ((first-ant (first map))
                  (ant-pos (first first-ant))
                  (ant-owner (third ant-pos)))
             (declare (type fixnum ant-owner))
             (cond ((or (and for-us (not (= 0 ant-owner)))
                        (and (not for-us) (= 0 ant-owner)))
                    (gen-positions (rest map) occupied
                                   force-nil-direction nil-first
                                   for-us
                                   cb so-far))
                   (t (let ((directions (mapcar #'first (rest first-ant))))
                        (when force-nil-direction
                          (when (not (member nil directions))
                            (cond (nil-first (push nil directions))
                                  (t (setf directions (nconc directions (list nil)))))))
                        (labels ((try-direction (dir)
                                   (multiple-value-bind (nr nc)
                                       (new-location (first ant-pos) (second ant-pos) dir)
                                     (when (= 0 (aref occupied nr nc))
                                       (setf (aref occupied nr nc) 1)
                                       (unwind-protect
                                            (gen-positions (rest map) occupied
                                                           force-nil-direction nil-first
                                                           for-us
                                                           cb (cons (get-hashed (list ant-pos dir))
                                                                    so-far))
                                         (setf (aref occupied nr nc) 0))))))
                          (dolist (dir directions)
                            (try-direction dir))))))))))

(defun position-is-not-acceptable (accept-losses our-losses their-losses)
  (declare (optimize speed)
           (type fixnum our-losses their-losses))
  (or (and (not accept-losses)
           (> our-losses 0))
      (and accept-losses
           ;; (< their-losses our-losses)
           (> our-losses 0)
           (= their-losses 0))))

(defvar *best-position*)

(defun calc-score (position)
  (declare (optimize speed (safety 0)))
  (let ((result 0s0))
    (declare (type single-float result))
    (dolist (pos position)
      (let ((coords (first pos))
            (dir (second pos)))
        (multiple-value-bind (nr nc)
            (new-location (first coords) (second coords) dir)
          (incf result (the single-float (score-at nr nc))))))
    result))

(defun gen-eval-positions (the-map accept-losses tactic)
  (declare (optimize speed (safety 0)))
  (let ((occupied (make-array '(200 200) :element-type '(mod 2) :initial-element 0))
        (total-positions-evaluated 0)
        (best-kills 0s0)
        (best-losses 0)
        (best-score 0s0))
    (declare (type fixnum total-positions-evaluated best-losses)
             (type single-float best-score best-kills))
    ;; (log-line "accepted losses: " accept-losses)
    (log-line "the map " the-map)
    (setf *best-position* nil)
    (check-time)
    (gen-positions
     the-map occupied t nil t
     (lambda (our-pos)
       #+extra-logging (log-line "evaluating: us: " our-pos)
       (let ((position-count 0)
             (kills 0)
             (their-kills-max 0)
             (position-is-acceptable t))
         (declare (type fixnum position-count kills their-kills-max))
         (block their-move
           (gen-positions
            the-map occupied t t nil
            (lambda (their-pos)
              #+extra-logging (log-line "          them: " their-pos)
              (incf position-count)
              (locally (declare (optimize speed))
                (incf total-positions-evaluated)
                (when (= 0 (rem total-positions-evaluated 256))
                  (check-time)))
              (multiple-value-bind (our-losses their-losses)
                  (eval-position our-pos their-pos the-map)
                #+extra-logging (log-line "        our losses: " our-losses
                                          " their losses " their-losses)
                (when (position-is-not-acceptable accept-losses our-losses their-losses)
                  (setf position-is-acceptable nil)
                  (return-from their-move))
                (incf kills (the fixnum their-losses))
                (setf their-kills-max (max their-kills-max (the fixnum our-losses)))))))
         (when position-is-acceptable
           (let ((candidate-kills (/ (float kills) position-count))
                 (candidate-losses their-kills-max)
                 (candidate-score (the single-float (calc-score our-pos))))
             (labels ((set-best ()
                        (setf *best-position* our-pos)
                        (setf best-kills candidate-kills)
                        (setf best-losses candidate-losses)
                        (setf best-score candidate-score))
                      (equal-kills ()
                        (< (abs (- candidate-kills best-kills)) single-float-epsilon))
                      (equal-score ()
                        (< (abs (- candidate-score best-score)) single-float-epsilon)))
               (ecase tactic
                 ((:defend)
                  (when (or (null *best-position*)
                            (< candidate-losses best-losses)
                            (and (= candidate-losses best-losses)
                                 (> candidate-kills best-kills)))
                    (set-best)))
                 ((:explore)
                  (when (or (null *best-position*)
                            (> candidate-score best-score)
                            (and (equal-score)
                                 (< candidate-losses best-losses))
                            (and (equal-score)
                                 (= candidate-losses best-losses)
                                 (> candidate-kills best-kills)))
                    (set-best)))
                 ((:explore-hard)
                  (when (or (null *best-position*)
                            (> candidate-score best-score)
                            (and (equal-score)
                                 (> candidate-kills best-kills))
                            (and (equal-score)
                                 (equal-kills)
                                 (< candidate-losses best-losses)))
                    (set-best)))
                 ((:attack-hard)
                  (when (or (null *best-position*)
                            (> candidate-kills best-kills)
                            (and (equal-kills)
                                 (> candidate-score best-score))
                            (and (equal-kills)
                                 (equal-score)
                                 (< candidate-losses best-losses)))
                    (set-best)))
                 ((:attack)
                  (when (or (null *best-position*)
                            (> candidate-kills best-kills)
                            (and (equal-kills)
                                 (< candidate-losses best-losses))
                            (and (equal-kills)
                                 (= candidate-losses best-losses)
                                 (> candidate-score best-score)))
                    (set-best))))))))))
    *best-position*))

;;; Configure battle maps.

(defun fill-in-square (battle-map row col ant dir)
  (let ((content (aref battle-map row col))
        (ant-coords (get-hashed (list (row-now ant) (col-now ant) (owner ant)))))
    (dolist (ant-in-content content)
      (when (eql ant-coords (first ant-in-content))
        (push dir (rest ant-in-content))
        ;; (log-line "!!! " row " " col " " dir " " (aref battle-map row col))
        ;; (dump-and-reset-log)
        (return-from fill-in-square)))
    (setf (aref battle-map row col)
          (cons (list ant-coords dir)
                content))
    ;; (log-line "!!! " row " " col " " dir " " (aref battle-map row col))
    ;; (dump-and-reset-log)
    ))

(defun fill-in-battle-map (battle-map ants)
  (declare (optimize speed))
  ;; (log-line "/// fill in battle map")
  (dolist (ant ants)
    (dolist (dir '(nil :north :east :south :west))
      (multiple-value-bind (nr nc)
          (new-location (row-now ant) (col-now ant) dir)
        (when (accessiblep nr nc)
          (look-around (list (list nr nc))
                       (lacb
                         (and (accessiblep r c)
                              (<= (the fixnum (distance r c nr nc))
                                  (the fixnum (attack-radius2 *state*)))))
                       (lacb
                         (fill-in-square battle-map r c ant dir))))))))

;;; Read information from battle maps.

(defun filter-battle-map (ant-owner)
  (lambda (enemy)
    (destructuring-bind ((row col owner) &rest dirs) enemy
      (declare (ignore row col dirs))
      (= ant-owner owner))))

(defun get-seen-enemies (battle-map ant)
  (let ((enemies (list (get-hashed (list (row-now ant) (col-now ant) (owner ant))))))
    (dolist (dir '(nil :north :east :south :west))
      (multiple-value-bind (nr nc)
          (new-location (row-now ant) (col-now ant) dir)
        (when (accessiblep nr nc)
          (awhen (remove-if (filter-battle-map (owner ant))
                            (aref battle-map nr nc))
            (push (get-hashed (cons dir it)) enemies)))))
    (nreverse enemies)))

(defun get-seen-enemies-list (battle-map ant)
  (declare (optimize debug))
  (let (result)
    (dolist (dir '(nil :north :east :south :west))
      (multiple-value-bind (nr nc)
          (new-location (row-now ant) (col-now ant) dir)
        (when (accessiblep nr nc)
          ;; (log-line "!!! " nr " " nc " " (aref battle-map nr nc))
          (dolist (enemy (-> (aref battle-map nr nc)
                             (remove-if (filter-battle-map (owner ant)) $)
                             (mapcar #'first $)))
            (pushnew enemy result :test #'eql))
          ;; (log-line "!!! " result)
          )))
    result))

;;; Clustering friendly ants.

(defun get-my-ants-in-battles (battle-map ants)
  (let (result)
    (dolist (ant ants)
      (when (and (= 0 (owner ant))
                 (> (length (get-seen-enemies battle-map ant))
                    1))
        (push ant result)))
    result))

(defun shuffle (ants)
  (let ((result (copy-list ants))
        (len (length ants)))
    (loop
       for i from 0 to (1- len)
       do (rotatef (elt result i) (elt result (random len))))
    result))

(defun find-one-cluster (ant ants processed-ants ants-enemies)
  (let ((cluster (list (list ant)
                       (gethash ant ants-enemies)))
        added-something)
    (setf (gethash ant processed-ants) t)
    ;; (log-line "starting cluster with ant " (strant ant))
    ;; (log-line "other ants " (mapcar #'strant ants))
    (loop
       (setf added-something nil)
       (dolist (other-ant ants)
         (unless (gethash other-ant processed-ants)
           ;; (log-line "us " (mapcar #'strant (first cluster)))
           ;; (log-line "them " (second cluster))
           ;; (log-line "enemies for " (strant other-ant) " " (gethash other-ant ants-enemies))
           (when (intersection (second cluster)
                               (gethash other-ant ants-enemies)
                               :test #'eql)
             (push other-ant (first cluster))
             (setf added-something t)
             (setf (second cluster) (union (second cluster)
                                           (gethash other-ant ants-enemies)
                                           :test #'eql))
             (setf (gethash other-ant processed-ants) t))))
       (unless added-something (return)))
    cluster))

(defun find-clusters (battle-map my-ants)
  (let ((processed-ants (make-hash-table))
        (ants-enemies (make-hash-table))
        clusters)
    (dolist (ant my-ants)
      (setf (gethash ant ants-enemies)
            (get-seen-enemies-list battle-map ant)))
    (dolist (ant my-ants)
      (unless (gethash ant processed-ants)
        (push (find-one-cluster ant my-ants processed-ants ants-enemies)
              clusters)))
    (reverse clusters)))

(defun get-cluster-center (cluster)
  (declare (optimize speed (safety 0)))
  (let ((center-row 0)
        (center-col 0)
        (count 0))
    (declare (type fixnum count center-row center-col))
    (dolist (ant (first cluster))
      (incf count)
      (incf center-row (the fixnum (row-now ant)))
      (incf center-col (the fixnum (col-now ant))))
    (setf center-row (round (/ center-row count)))
    (setf center-col (round (/ center-col count)))
    (let* ((best-ant (-> cluster first first))
           (best-distance (distance (row-now best-ant) (col-now best-ant)
                                    center-row center-col)))
      (dolist (ant (-> cluster first rest))
        (let ((distance (distance (row-now ant) (col-now ant)
                                  center-row center-col)))
          (when (< (the fixnum distance) (the fixnum best-distance))
            (setf best-ant ant)
            (setf best-distance distance))))
      (values (row-now best-ant) (col-now best-ant)))))

;;; Battle analysis driver.

(defun strant (ant)
  (format nil "(~a ~a)" (row-now ant) (col-now ant)))

(defun get-tactic (cluster)
  (multiple-value-bind (row col)
      (get-cluster-center cluster)
    (multiple-value-bind (us-count them-count)
        (count-us-them row col (* 2 (view-radius *state*)))
      (let ((upper-hand (>= us-count (* 3 them-count)))
            (lower-hand (<= us-count them-count)))
        (when lower-hand
          (return-from get-tactic :defend))
        (let ((max-dist (* 3 (view-radius *state*))))
          (look-around (list (list row col))
                       (lacb
                         (accessiblep r c)
                         (<= dist max-dist))
                       (lacb
                         (awhen (get-hill-at r c)
                           (cond ((and (= 0 (owner it))
                                       (<= dist (view-radius *state*)))
                                  (return-from get-tactic :defend))
                                 ((not (= 0 (owner it)))
                                  (return-from get-tactic (if upper-hand
                                                              :attack-hard
                                                              :attack))))))))
        (if upper-hand :explore-hard :explore)))))

(defun analyze-cluster (cluster battle-map)
  ;; Generate all the moves; analyze them; if we find a configuration
  ;; that works, return it, else return nil.
  (log-line "/// cluster: us " (mapcar #'strant (first cluster))
            " them " (second cluster))
  (let* ((the-map (append (mapcar (lambda (ant)
                                    (get-seen-enemies battle-map ant))
                                  (first cluster))
                          (mapcar (lambda (ant)
                                    (get-seen-enemies battle-map
                                                      (get-ant-at (first ant) (second ant))))
                                  (second cluster)))))
    (catch 'finish-turn
      (gen-eval-positions the-map
                          (> (-> cluster first length) 2)
                          (get-tactic cluster)))
    (log-line "/// best-position " *best-position*)
    (when *best-position*
      (list *best-position*))))

(defun get-battle-map (ants &optional battle-map)
  (when (null battle-map)
    (setf battle-map
          (make-array (list (rows *state*) (cols *state*)) :initial-element nil)))
  (fill-in-battle-map battle-map ants)
  battle-map)

(defun analyze-battles (ants)
  (let* ((enemy-ants (remove-if (lambda (ant) (= 0 (owner ant)))
                                ants))
         (enemy-battle-map (timed (get-battle-map enemy-ants)))
         (my-ants-in-battles (timed (get-my-ants-in-battles enemy-battle-map ants))))
    (log-line "enemies count " (length enemy-ants))
    (log-line "my ants in battles count " (length my-ants-in-battles))
    (let ((battle-map (timed (get-battle-map my-ants-in-battles enemy-battle-map))))
      #+extra-logging
      (log-line "my ants in battles " (mapcar #'strant my-ants-in-battles))
      (let* ((clusters (timed (find-clusters battle-map my-ants-in-battles)))
             (sorted-clusters (-> clusters
                                  (mapcar (lambda (cluster)
                                            (list cluster (+ (->  cluster first length)
                                                             (-> cluster second length))))
                                          $)
                                  (sort $ #'< :key #'second)
                                  (mapcar #'first $)))
             result)
        (dolist (cluster sorted-clusters)
          (let ((analyzed (timed (analyze-cluster cluster battle-map))))
            (when analyzed
              (push (first analyzed) result)
              (dolist (ant (first cluster))
                (setf (is-in-cluster ant) t)))
            (when (soft-check-time)
              (return))))
        result))))
