
#-aichallenge (declaim (optimize (debug 3) (speed 0) (safety 3)))
#+aichallenge (declaim (optimize (debug 0) (speed 1) (safety 0)))

(defun par-value (string)
  "Helper function for parsing game state input from the server."
  (parse-integer (subseq string (position #\space string) (length string))))

(defun starts-with (sequence subsequence)
  (let ((sublen (length subsequence)))
    (when (and (> sublen 0)
               (<= sublen (length sequence)))
      (equal (subseq sequence 0 sublen) subsequence))))

(defun initialize-state ()
  (setf (seen-food *state*) (make-hash-table :test #'equal))
  ;; (setf (battles-map *state*)
  ;;       (make-array (list (rows *state*) (cols *state*))
  ;;                   :element-type 'fixnum
  ;;                   :initial-element 0))
  (setf (terrain-map *state*)
        (make-array '(200 200)
                    :element-type '(mod 4)
                    :initial-element 0))
  (setf (hills-map *state*)
        (make-array (list (rows *state*) (cols *state*))
                    :initial-element nil))
  (setf (fog-map *state*)
        (make-array '(200 200)
                    :element-type 'fixnum
                    :initial-element 1))
  (setf (rush-target *state*) nil)
  (setf (all-my-hills *state*) nil))

(defun parse-game-parameters ()
  "Parses turn 0 game parameters and sets them in *STATE*.  Also creates
  initial game map and assigns it to (GAME-MAP *STATE*)."
  (setf *state* (make-instance 'state))
  (loop for line = (read-line *standard-input* nil)
     until (starts-with line "ready")
     do (cond ((starts-with line "attackradius2 ")
               (setf (slot-value *state* 'attack-radius2) (par-value line))
               (setf (slot-value *state* 'attack-radius) (-> line par-value sqrt floor)))
              ((starts-with line "cols ")
               (setf (slot-value *state* 'cols) (par-value line))
               (setf *cols* (par-value line)))
              ((starts-with line "loadtime ")
               (setf (slot-value *state* 'load-time)
                     (/ (par-value line) 1000.0)))
              ((starts-with line "rows ")
               (setf (slot-value *state* 'rows) (par-value line))
               (setf *rows* (par-value line)))
              ((starts-with line "spawnradius2 ")
               (setf (slot-value *state* 'spawn-radius2) (par-value line)))
              ((starts-with line "turns ")
               (setf (slot-value *state* 'turns) (par-value line)))
              ((starts-with line "turntime ")
               (setf (slot-value *state* 'turn-time)
                     (/ (max 1000 (par-value line))
                        ;;(par-value line)
                        1000.0)))
              ((starts-with line "viewradius2 ")
               (setf (slot-value *state* 'view-radius2) (par-value line))
               (setf (slot-value *state* 'view-radius) (-> line par-value sqrt floor)))))
  (initialize-state))

(defun update-fog-map ()
  (declare (optimize speed (safety 0)))
  (let ((size-rows (rows *state*))
        (size-cols (cols *state*)))
    (declare (type (integer 1) size-rows size-cols))
    (fast-operate-on (fog-map *state*) fixnum
      (mapmap 0 0
              (1- (the fixnum size-rows)) (1- (the fixnum size-cols))
              (lambda (ir ic)
                (declare (type fixnum ir ic))
                (setf (aref map ir ic)
                      (the fixnum (1+ (aref map ir ic)))))))))

(defun turn-start-reset-maps ()
  (setf (lists-hash *state*) (make-hash-table :test #'equal))
  (setf (cluster-moves *state*) nil)
  (update-fog-map)
  (setf (ants-map *state*)
        (make-array '(200 200)
                    :initial-element nil))
  (setf (ant-occupied-map *state*)
        (make-array (list (rows *state*) (cols *state*))
                    :initial-element nil))
  (setf (food-map *state*)
        (make-array '(200 200)
                    :element-type '(mod 2)
                    :initial-element 0))
  (setf (hotspot-map *state*)
        (make-array '(200 200)
                    :element-type 'single-float
                    :initial-element 0s0))
  #-aichallenge (setf (hmap-explain *state*)
                      (make-array (list (rows *state*) (cols *state*))
                                  :initial-element nil))
  (setf (defensive-hotspot-map *state*)
        (make-array '(200 200)
                    :element-type 'single-float
                    :initial-element 0s0))
  ;; (setf (hill-distance-map *state*)
  ;;       (make-array (list (rows *state*) (cols *state*))
  ;;                   :element-type 'fixnum
  ;;                   :initial-element 10000))
  (setf *height-map* (hotspot-map *state*))
  (setf (food-list *state*) nil)
  (setf (hotspots *state*) nil)
  (setf (my-hills *state*) nil)
  (setf (ants *state*) nil)
  (setf (applied-hotspots *state*) nil))

(defun split-state-string (string)
  (loop with result = nil
     with value = nil
     for c across string
     when (and (char= c #\space) value)
     do (push (coerce (nreverse value) 'string) result)
       (setf value nil)
     when (char/= c #\space)
     do (push c value)
     finally (when value
               (push (coerce (nreverse value) 'string) result))
       (return (nreverse result))))

(defun set-ant (string)
  ;; (log-line "set-ant " string)
  "Parses the \"a row col owner\" STRING and sets the specific map tile to
  an ant of owner.  Modifies (ENEMY-ANTS *STATE*), (MY-ANTS *STATE*) and
  (GAME-MAP *STATE*)."
  (let* ((split (split-state-string string))
         (row (parse-integer (elt split 1)))
         (col (parse-integer (elt split 2)))
         (owner (parse-integer (elt split 3)))
         (result (set-ant-at row col owner)))
    (occupy-at row col)
    (when (= owner 0)
      (push (list row col) (ants *state*))
      (mark-unknown-as-land-at row col))
    (set-land-at row col)
    result))

(defun set-food (string)
  ;; (log-line "set-food " string)
  "Parses the \"f row col\" STRING and sets the specific map tile to food.
  Modifies (FOOD *STATE*) and (GAME-MAP *STATE*)."
  (let* ((split (split-state-string string))
         (row (parse-integer (elt split 1)))
         (col (parse-integer (elt split 2))))
    (setf (gethash (list row col) (seen-food *state*)) t)
    (set-land-at row col)
    (set-food-at row col)))

(defun set-water (string)
  ;; (log-line "set-water " string)
  "Parses the \"w row col\" STRING and sets the specific map tile to water.
  Modifies (GAME-MAP *STATE*)."
  (let* ((split (split-state-string string))
         (row (parse-integer (elt split 1)))
         (col (parse-integer (elt split 2))))
    (set-water-at row col)))

(defun initialize-hill (hill)
  (let (locations)
    (loop
       for ir from (inc-row (row hill) -1) to (inc-row (row hill) 1)
       do (loop
             for ic from (inc-col (col hill) -1) to (inc-col (col hill) 1)
             when (and (landp ir ic)
                       (not (and (= ir (row hill))
                                 (= ic (col hill)))))
             do (push (list ir ic nil) locations)))
    (setf (to-protect hill) locations)
    (log-line "to protect at " (row hill) " " (col hill) " " locations)))

;; TODO detect the razing of hills
(defun set-hill (string)
  ;; (log-line "set hill " string)
  "Parses the \"a row col owner\" STRING and sets the specific map tile to
  a hill of owner.  Modifies (HILLS *STATE*) and (GAME-MAP *STATE*)."
  (let* ((split (split-state-string string))
         (row (parse-integer (elt split 1)))
         (col (parse-integer (elt split 2)))
         (owner (parse-integer (elt split 3)))
         (hill (make-instance 'hill
                              :row row
                              :col col
                              :standing t
                              :owner owner)))
    (set-land-at row col)
    (set-hill-at row col hill)
    (list row col owner)))

(defun issue-order (row col dir)
  (when (member dir '(:north :east :south :west))
    (format *standard-output* "~&o ~D ~D ~A~%" row col
            (case dir
              (:north "N")
              (:east  "E")
              (:south "S")
              (:west  "W")))))

#-aichallenge
(defun opacity-for (ir ic)
  (if (fogp ir ic) 0.5 1.0))

#-aichallenge
(defun draw-map-to-png ()
  (let ((header (list (turn *state*) (cols *state*) (rows *state*)))
        content)
    (mapmap 0 0
            (the fixnum (1- (the fixnum (rows *state*))))
            (the fixnum (1- (the fixnum (cols *state*))))
            (lambda (ir ic)
              (declare (type fixnum ir ic))
              (when (landp ir ic)
                (push (list :draw-big-pixel ic ir 0.8509804 0.81960785 0.4862745
                            (opacity-for ir ic))
                      content))
              (when (waterp ir ic)
                (push (list :draw-big-pixel ic ir 0.1254902 0.38431373 0.8156863
                            (opacity-for ir ic))
                      content))))
    (mapmap 0 0
            (the fixnum (1- (the fixnum (rows *state*))))
            (the fixnum (1- (the fixnum (cols *state*))))
            (lambda (ir ic)
              (bwhen (hill (get-hill-at ir ic))
                (cond ((not (standing hill))
                       (push (list :draw-big-pixel ic ir 1.0 1.0 1.0 (opacity-for ir ic))
                             content))
                      ((= 0 (the fixnum (owner hill)))
                       (push (list :draw-extra-big-pixel ic ir
                                   0.21568628 0.8980392 0.105882354
                                   (opacity-for ir ic))
                             content))
                      (t (push (list :draw-extra-big-pixel ic ir
                                     0.9372549 0.19215687 0.2784314
                                     (opacity-for ir ic))
                               content))))))
    (mapmap 0 0
            (the fixnum (1- (the fixnum (rows *state*))))
            (the fixnum (1- (the fixnum (cols *state*))))
            (lambda (ir ic)
              (bwhen (ant (get-ant-at ir ic))
                (cond ((= 0 (the fixnum (owner ant)))
                       (push (list :draw-big-pixel ic ir
                                   0.08627451 0.50980395 0.105882354
                                   (opacity-for ir ic))
                             content))
                      (t (push (list :draw-big-pixel ic ir
                                     0.63529414 0.06666667 0.17254902
                                     (opacity-for ir ic))
                               content))))
              (when (foodp ir ic)
                (push (list :draw-big-pixel ic ir
                            0.105882354 0.7921569 0.8039216
                            (opacity-for ir ic))
                      content))))
    (dolist (hotspot (applied-hotspots *state*))
      (setf content
            (nconc (mapcar (lambda (coord)
                             (list :draw-rectangle (first coord) (second coord) 0.5 0.5 0.5 1.0))
                           (coords hotspot))
                   content)))
    (img-put-queue (nconc header content))))

(defun find-hotspots ()
  (multiple-value-bind (new-hotspots cells-explored)
      (look-for-hotspots (all-my-hills *state*) (hotspots *state*))
    (declare (ignorable cells-explored))
    (log-line "cells explored looking for hotspots " cells-explored)
    (setf (hotspots *state*) new-hotspots))
  (log-line "found a total of " (length  (hotspots *state*)) " hotspots")
  (setf (hotspots *state*)
        (-> (hotspots *state*)
            (sort $ #'> :key #'strength)))
  #+(and (not aichallenge) text-logging)
  (dolist (hotspot (hotspots *state*))
    (log-line (strpot hotspot)))
  (log-line "time remaining after hotspot search: " (turn-time-remaining)))

(defun apply-basic-hotspots (food-spec)
  (declare (optimize speed (safety 0)))
  (let ((applied-hotspots-count 0))
    (declare (type fixnum applied-hotspots-count))
    ;; (log-line "apply-basic-hotspots food-spec: " food-spec)
    (dolist (hotspot (hotspots *state*))
      ;; (log-line "considering applying hotspot " (strpot hotspot))
      (let* ((is-food (eq :food (kind hotspot)))
             (should-apply (or (and (eq :exclude-food food-spec)
                                    (not is-food))
                               (and (eq :only-food food-spec)
                                    is-food))))
        (when should-apply
          (log-line "remaining time " (turn-time-remaining))
          (timed (apply-hotspot hotspot))
          (incf applied-hotspots-count))))
    (log-line "applied hotspots count " applied-hotspots-count)))

(defun apply-friendly-ant (ant)
  (declare (ignorable ant))
  ;; (let ((height (aref *height-map* (row-now ant) (col-now ant))))
  ;;   (if (> height 0)
  ;;       (setf (aref *height-map* (row-now ant) (col-now ant)) 0s0)))
  )

(defun apply-ants-to-map (ants)
  (dolist (ant ants)
    (when (not (= 0 (owner ant)))
      (apply-enemy-ant ant))))

(defun dont-stay-on-hills ()
  (dolist (hill (my-hills *state*))
    (setf (aref (hotspot-map *state*) (row hill) (col hill)) -10000s0)
    (setf (aref (defensive-hotspot-map *state*) (row hill) (col hill)) -10000s0)))

(defun count-ants-around (row col distance enemy)
  (declare (optimize speed (safety 0))
           (type fixnum distance))
  (let ((count 0))
    (declare (type fixnum count))
    (look-around (list (list row col))
                 (lacb
                   (declare (type fixnum dist))
                   (and (accessiblep r c)
                        (<= dist distance)))
                 (lacb
                   (awhen (get-ant-at r c)
                     (when (or (and (not enemy) (= 0 (the fixnum (owner it))))
                               (and enemy (not (= 0 (the fixnum (owner it))))))
                       (incf count)))))
    count))

(defun count-friends-around (row col)
  (count-ants-around row col (* 2 (view-radius *state*)) nil))

(defun count-friend-around-close (row col)
  (count-ants-around row col 2 nil))

(defun count-enemies-around (row col)
  (count-ants-around row col (* 2 (view-radius *state*)) t))

(defun defend-hills (ants)
  (let* ((my-ants-count (length (remove-if-not (lambda (ant)
                                                 (= 0 (owner ant)))
                                               ants)))
         (my-standing-hills (remove-if-not #'standing (all-my-hills *state*)))
         (my-standing-hills-count (length my-standing-hills)))
    (dolist (hill my-standing-hills)
      (let ((enemies-around (count-enemies-around (row hill) (col hill)))
            (friends-around (count-friends-around (row hill) (col hill))))
        (log-line "/// defend-hills")
        (log-line "enemies around " enemies-around
                  " friends around " friends-around
                  " friends very close " (count-friend-around-close (row hill) (col hill)))
        (let ((friends-close (count-friend-around-close (row hill) (col hill))))
          (when (and (> enemies-around 0)
                     (or (< friends-around enemies-around)
                         (< friends-close 2))
                     (> my-ants-count (* 2 my-standing-hills-count)))
            (let ((*height-map* (defensive-hotspot-map *state*)))
              (apply-hotspot (make-instance 'hotspot
                                            :coords (list (list (row hill) (col hill)))
                                            :strength 10s0
                                            :radius (* 2 my-standing-hills-count)
                                            :kind :defense
                                            :description "defense"))))
          (let ((blocked-number (cond ((and (> enemies-around 0)
                                            (>= friends-close 2)
                                            (> my-ants-count (* 3 my-standing-hills-count)))
                                       2)
                                      ((and (>= friends-close 1)
                                            (> my-ants-count (* 2 my-standing-hills-count))
                                            (> (turn *state*) 15))
                                       1)
                                      (t 0))))
            (when (> blocked-number 0)
              (block block-ants
                (let ((counter blocked-number))
                  (look-around (list (list (row hill) (col hill)))
                               (lacb
                                 (and (accessiblep r c)
                                      (<= dist 2)))
                               (lacb
                                 (unless (and (= r (row hill)) (= c (col hill)))
                                   (awhen (get-ant-at r c)
                                     (when (= 0 (owner it))
                                       (setf (is-processed it) t)
                                       (log-line "blocked ant " (strant it))
                                       (decf counter)
                                       (when (= 0 counter)
                                         (return-from block-ants))))))))))))))))


(defun clean-eaten-food ()
  (maphash (lambda (k v)
             (declare (ignore v))
             (let ((r (first k))
                   (c (second k)))
               (when (and (not (fogp r c))
                          (not (foodp r c)))
                 (remhash k (seen-food *state*)))))
           (seen-food *state*)))

(defun clean-hotspots ()
  (declare (optimize speed))
  (let ((clean-array (make-array '(40000)
                                 :element-type 'single-float
                                 :displaced-to *height-map*)))
    (fill clean-array 0s0)))

(defun parse-turn ()
  "Parses a typical turn.  Modifies *STATE* indirectly through RESET-GAME-MAP
  and the SET-* functions."
  (turn-start-reset-maps)
  (log-line "*************************** turn " (turn *state*))
  (timed (sb-ext:gc))
  ;; (log-line "terrain is " (terrain-map *state*))
  (let (seen-hills ants)
    (loop for line = (read-line *standard-input* nil)
       until (starts-with line "go")
       do (cond ((starts-with line "f ") (set-food line))
                ((starts-with line "w ") (set-water line))
                ((starts-with line "a ") (push (set-ant line) ants))
                ((starts-with line "d "))
                ((starts-with line "h ")
                 (push (set-hill line) seen-hills))))
    (clean-eaten-food)
    (dolist (hill seen-hills)
      (set-hill-standing-at (first hill) (second hill) t)
      (awhen (get-hill-at (first hill) (second hill))
        (if (= 0 (owner it))
            (push it (my-hills *state*)))))
    (unless (all-my-hills *state*)
      (setf (all-my-hills *state*) (copy-list (my-hills *state*))))
    (let ((*cutoff-time* 0.1s0))
      (catch 'finish-turn
        (timed (defend-hills ants))
        (timed (find-hotspots))
        (timed (apply-basic-hotspots :exclude-food))
        ;; (timed (apply-basic-hotspots :only-food))
        (let ((*cutoff-time* (/ (turn-time-remaining) 5)))
          (catch 'finish-turn
            (setf (cluster-moves *state*) (timed (analyze-battles ants)))))
        ;; (timed (clean-hotspots))
        ;; (timed (apply-basic-hotspots :exclude-food))
        (timed (apply-basic-hotspots :only-food))
        (timed (apply-ants-to-map ants))
        ;; (timed (dont-stay-on-hills))
        (log-line "time remaining after setting up hotspot map: " (turn-time-remaining))
        #+visual-logging (timed (draw-map-to-png))
        ))))

(defun parse-game-state ()
  "Calls either PARSE-TURN or PARSE-GAME-PARAMETERS depending on the line
  on standard input.  Modifies *STATE* and returns T if the game has ended,
  otherwise NIL."
  (setf (slot-value *state* 'turn-start-time) (wall-time))
  (loop for line = (read-line *standard-input* nil)
     until (> (length line) 0)
     finally (return (cond ((starts-with line "end")
                            (parse-turn)
                            t)
                           ((starts-with line "turn 0")
                            (setf (slot-value *state* 'turn) 0)
                            (parse-game-parameters)
                            nil)
                           ((starts-with line "turn ")
                            (setf (slot-value *state* 'turn)
                                  (par-value line))
                            (parse-turn)
                            nil)))))

(defun user-interrupt (arg)
  (declare (ignore arg))
  (format *debug-io* "~&User interrupt. Aborting...~%")
  (quit))

(defun issue-orders ()
  (let ((occupied-next #-aichallenge (make-hash-table :test #'equal)))
    (declare (ignorable occupied-next))
    (dolist (ant-coords (ants *state*))
      (let ((ant (get-ant-at (first ant-coords) (second ant-coords))))
        (log-line "order at " (row-now ant) " " (col-now ant) " is " (order ant))
        (when (order ant)
          (issue-order (row-now ant) (col-now ant) (order ant)))
        #-aichallenge
        (multiple-value-bind (nr nc)
            (new-location (row-now ant) (col-now ant) (order ant))
          (unless (landp nr nc)
            (setf nr (row-now ant))
            (setf nc (col-now ant)))
          (let ((this-ant (list (row-now ant) (col-now ant) (order ant))))
            (awhen (gethash (list nr nc) occupied-next)
              (log-line "boohoo kill at " nr " " nc)
              (log-line "conflict between " it)
              (log-line "and " this-ant))
            (setf (gethash (list nr nc) occupied-next) this-ant)))))))

(defun finish-turn ()
  "Prints the \"finish turn\" string to standard output."
  (issue-orders)
  (when (turn *state*)
    (log-line "remaining time " (turn-time-remaining)))
  (format *standard-output* "~&go~%")
  (force-output *standard-output*)
  #-aichallenge (dump-and-reset-log))
