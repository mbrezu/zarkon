
#-aichallenge (declaim (optimize (debug 3) (speed 0) (safety 3)))
#+aichallenge (declaim (optimize (debug 0) (speed 1) (safety 0)))

(defun look-around (start-locations validp action)
  (declare (optimize speed (safety 0)))
  (let* ((visited (make-array '(200 200) :element-type '(mod 2) :initial-element 0))
         (queue (let ((result (tl-make-empty)))
                  (mapc (lambda (pos)
                          (let ((row (first pos))
                                (col (second pos)))
                            (tl-push-back result (list row col 0))
                            (setf (aref visited row col) 1)))
                        start-locations)
                  result))
         (processed-cells 0))
    (declare (type fixnum processed-cells)
             (type (function (fixnum fixnum fixnum) t) validp action))
    (check-time)
    (loop
       while (not (tl-empty queue))
       do (let ((qhead (tl-pop-front queue)))
            ;; (log-line "queue head is " qhead)
            (destructuring-bind (current-row current-col distance)
                qhead
              (declare (type fixnum distance))
              (incf processed-cells)
              (when (= 512 processed-cells)
                (check-time)
                (setf processed-cells 0))
              (funcall action current-row current-col distance)
              (let (new-locations)
                (dolist (dir '(:north :south :east :west))
                  (multiple-value-bind (pos-row pos-col)
                      (new-location current-row current-col dir)
                    (when (and (= (aref visited pos-row pos-col) 0)
                               (funcall validp pos-row pos-col (the fixnum (1+ distance))))
                      (push (list pos-row pos-col (the fixnum (1+ distance)))
                            new-locations))))
                (dolist (loc new-locations)
                  (setf (aref visited (first loc) (second loc)) 1)
                  (tl-push-back queue loc))))))))

(defmacro lacb (&body body)
  `(lambda (r c dist)
     (declare (ignorable r c dist))
     ,@body))

(defun extend-cluster (cluster battle-hash)
  (dolist (ant cluster)
    (look-around (list (list (row-now ant) (col-now ant)))
                 (lacb (and (accessiblep r c)
                            (< dist 15)))
                 (lacb
                   (awhen (get-ant-at r c)
                     (when (and (not (= 0 (owner it)))
                                (not (gethash it battle-hash)))
                       ;; (log-line "/// extending cluster with ant at "
                       ;;           (row-now it) " " (col-now it))
                       (return-from extend-cluster it)))))))

(defun count-us-them (row col dist-limit)
  (declare (optimize speed (safety 0)))
  (let ((us-count 0)
        (them-count (make-hash-table :test #'eql))
        (max-them-count 0))
    (declare (type fixnum us-count dist-limit max-them-count))
    (look-around (list (list row col))
                 (lacb
                   (declare (type fixnum dist))
                   (and (accessiblep r c)
                        (<= dist dist-limit)))
                 (lacb
                   (bwhen (ant (get-ant-at r c))
                     (let ((owner (owner ant)))
                       (declare (type fixnum owner))
                       (cond ((= 0 owner) (incf us-count))
                             (t (unless (gethash owner them-count)
                                  (setf (gethash owner them-count) 0))
                                (incf (the fixnum (gethash owner them-count)))))))))
    (maphash (lambda (k v)
               (declare (ignore k)
                        (type fixnum v))
               (when (> v max-them-count)
                 (setf max-them-count v)))
             them-count)
    (values us-count max-them-count)))

(defun apply-hotspot (hotspot)
  (declare (optimize speed))
  (log-line "/// applying " (strpot hotspot))
  (push hotspot (applied-hotspots *state*))
  (let ((stop-at-first-ant (eq :first-ant (stop-at hotspot)))
        (radius (radius hotspot)))
    (labels ((good-cell (r c dist)
               (declare (type fixnum r c dist))
               (and (landp r c)
                    (or (null radius)
                        (<= dist (the fixnum radius)))
                    (not (aif (get-ant-at r c)
                              (= 0 (the fixnum (owner it))))))))
      (when (coords hotspot)
        (look-around (coords hotspot)
                     (lacb
                       (good-cell r c dist))
                     (lacb
                       (when (good-cell r c dist)
                         (let ((increment (cond ((= 0 (the fixnum dist)) (strength hotspot))
                                                (t (/ (the single-float (strength hotspot))
                                                      (let ((dist1 (+ 1s0 dist)))
                                                        (declare (type single-float dist1))
                                                        (the single-float (* dist1 dist1))))))))
                           (declare (type single-float increment))
                           #+extra-logging (push (list hotspot increment)
                                                 (aref (hmap-explain *state*) r c))
                           (locally
                               (declare (type (simple-array single-float (200 200)) *height-map*))
                             (incf (aref *height-map* r c)
                                   increment)))
                         (when (and stop-at-first-ant
                                    (aif (my-ants-neighbors r c)
                                         (some (lambda (ant) (not (or (is-processed ant)
                                                                      (is-in-cluster ant))))
                                               (the (cons ant) it))))
                           (return-from apply-hotspot)))))))))

(defun create-food-hotspots ()
  (let ((processed-food (make-hash-table :test #'equal))
        result)
    (maphash (lambda (k v)
               (declare (ignore v))
               (unless (gethash k processed-food)
                 (let ((food-coords (list k)))
                   (look-around (list k)
                                (lacb
                                  (and (landp r c)
                                       (foodp r c)
                                       (food-neighbors r c)))
                                (lacb
                                  (when (foodp r c)
                                    (let ((coords (list r c)))
                                      (push coords food-coords)
                                      (setf (gethash coords processed-food) t)))))
                   (push (make-instance 'hotspot
                                        :coords food-coords
                                        :stop-at :first-ant
                                        :strength (* (length food-coords)
                                                     1000s0)
                                        :kind :food
                                        :description "food")
                         result))))
             (seen-food *state*))
    result))

(defun update-rush-target ()
  (labels ((find-rush-target (row col)
             (look-around (list (list row col))
                          (lacb
                            (landp r c))
                          (lacb
                            (when (unknown-neighbors r c)
                              (setf (rush-target *state*) (list r c))
                              (return-from find-rush-target))))))
    (cond ((rush-target *state*)
           (let ((row (first (rush-target *state*)))
                 (col (second (rush-target *state*))))
             (when (not (unknown-neighbors row col))
               (find-rush-target row col))))
          (t (awhen (-> (all-my-hills *state*)
                        (remove-if-not #'standing $)
                        first)
               (find-rush-target (row it) (col it)))))))

(defun look-for-hotspots (hills hotspots)
  (declare (optimize speed (safety 0)))
  (let ((cells-explored 0)
        fog-coords
        unknown-coords
        hill-coords
        (has-hills (some #'standing (the (cons t) (all-my-hills *state*)))))
    (declare (type fixnum cells-explored))
    (log-line "has hills " has-hills)
    (look-around (mapcar (lambda (hill) (list (row hill) (col hill)))
                         hills)
                 (lacb
                   ;; (log-line "at " r " " c " " (or (accessiblep r c)
                   ;;                                 (foodp r c)))
                   (or (accessiblep r c)
                       (foodp r c)))
                 (lacb
                   (incf cells-explored)
                   (cond ((unrazed-enemy-hill-at r c)
                          (push (list r c) hill-coords))
                         ((unknown-neighbors r c)
                          (push (list r c) unknown-coords))
                         ((and has-hills
                               (> (the fixnum (fog-age r c)) 20)
                               (not-fog-neighbors r c))
                          (push (list r c) fog-coords)))))
    (when hill-coords
      (push (make-instance 'hotspot
                           :coords hill-coords
                           :strength 20000s0
                           :kind :enemy-hill
                           :description "enemy hill")
            hotspots))
    (when has-hills
      (dolist (hs (create-food-hotspots))
        (push hs hotspots)))
    (push (make-instance 'hotspot
                         :coords unknown-coords
                         :strength 400s0
                         :kind :unknown
                         :description "unknown")
          hotspots)
    (push (make-instance 'hotspot
                         :coords fog-coords
                         :strength 100s0
                         :kind :fog
                         :description "fog")
          hotspots)
    (values (nreverse hotspots) cells-explored)))

(defun apply-enemy-ant (ant)
  (labels ((forbid-area (row col)
             (mapmap-within-distance row col
                                     (attack-radius *state*) (attack-radius2 *state*)
                                     (lambda (r c)
                                       (when (landp r c)
                                         (forbid r c))))))
    (let ((row (row-now ant))
          (col (col-now ant)))
      (forbid-area row col)
      (dolist (dir '(:north :south :east :west))
        (multiple-value-bind (nr nc) (new-location row col dir)
          (when (landp nr nc)
            (forbid-area nr nc)))))))
