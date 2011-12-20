
;;;; uncomment this to submit
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *features* (pushnew :aichallenge *features*)))

;;;; uncomment this to activate visual logging
;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (setf *features* (pushnew :visual-logging *features*)))

;;;; uncomment this to get text logs
;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (setf *features* (pushnew :text-logging *features*)))

;;;; uncomment this to get a lot of information in the logs
;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (setf *features* (pushnew :extra-logging *features*)))


#-aichallenge (declaim (optimize (debug 3) (speed 0) (safety 3)))
#+aichallenge (declaim (optimize (debug 0) (speed 1) (safety 0)))

(defvar *cutoff-time*)

(setf *cutoff-time* 0.05s0)

(load "utils.lisp")
(load "classes.lisp")
(load "terrain.lisp")
(load "search.lisp")
(load "battle.lisp")
(load "input-parser.lisp")

(defun ant-mobility (row col &optional (possible-row row) (possible-col col))
  (let ((mobility 0))
    (dolist (dir '(:north :south :east :west))
      (multiple-value-bind (nr nc)
          (new-location row col dir)
        (when (or (and (accessiblep nr nc)
                       (not (occupied-at nr nc)))
                  (and (= nr possible-row)
                       (= nc possible-col)))
          (incf mobility))))
    mobility))

(defun get-best-move (ant)
  (let ((best-score (score-at (next-row ant) (next-col ant)))
        (best-mobility (ant-mobility (next-row ant) (next-col ant)))
        (best-directions (list (order ant))))
    #+extra-logging (log-line "best score is " best-score
                              " and best directions are " best-directions)
    #+extra-logging
    (when (= 2 (turn *state*))
      #+extra-logging
      (log-line "score explanation is " (mapcar (lambda (el)
                                                  (list (strpot (first el))
                                                        (second el)))
                                                (aref (hmap-explain *state*)
                                                      (next-row ant) (next-col ant)))))
    (dolist (dir '(nil :north :south :east :west))
      #+extra-logging (log-line "considering going " dir)
      (multiple-value-bind (nr nc)
          (new-location (row-now ant) (col-now ant) dir)
        ;; (log-line "at " nr " " nc
        ;;           " accessible " (accessiblep nr nc)
        ;;           " occupied " (occupied-at nr nc))
        (when (and (accessiblep nr nc)
                   (not (occupied-at nr nc)))
          (let ((score (score-at nr nc))
                (mobility (ant-mobility nr nc (row-now ant) (col-now ant))))
            #+extra-logging (log-line "score is " score)
            #-aichallenge
            (when (= 2 (turn *state*))
              #+extra-logging
              (log-line "score explanation is " (mapcar (lambda (el)
                                                          (list (strpot (first el))
                                                                (second el)))
                                                        (aref (hmap-explain *state*) nr nc))))
            (cond ((or (> score best-score)
                       (and (= score best-score)
                            (> mobility best-mobility)))
                   (setf best-score score)
                   (setf best-directions (list dir))
                   (setf best-mobility mobility))
                  ((and (= score best-score)
                        (= mobility best-mobility))
                   (push dir best-directions)))))))
    #+extra-logging (log-line "best score is " best-score
                              " best directions are " best-directions
                              " and best mobility is " best-mobility)
    (values best-score best-mobility best-directions)))

(defun move-ant (ant direction)
  (unoccupy-at (next-row ant) (next-col ant))
  (setf (order ant) direction)
  (multiple-value-bind (nr nc)
      (new-location (row-now ant) (col-now ant) (order ant))
    (setf (next-row ant) nr)
    (setf (next-col ant) nc)
    (occupy-at nr nc)))

(defun handle-ant (row col)
  (log-line "*** turn " (turn *state*) " handle-ant "
            ;; (get-ant-name row col)
            " at " row " " col )
  (log-line "*** turn time remaining " (turn-time-remaining))
  (check-time)
  (let* ((ant (get-ant-at row col)))
    (when (is-processed ant)
      (log-line "ant in some cluster")
      (return-from handle-ant t))
    (when (not (is-processed ant))
      (multiple-value-bind (best-score best-mobility best-directions)
          (let ((*height-map* (defensive-hotspot-map *state*)))
            (get-best-move ant))
        (declare (ignore best-mobility))
        (cond ((and (> best-score 0s0) best-directions)
               (log-line "using the defensive map")
               (move-ant ant (random-elt best-directions))
               (>= best-score 0s0))
              (t (multiple-value-bind (best-score best-mobility best-directions)
                     (get-best-move ant)
                   (declare (ignore best-mobility))
                   (when best-directions
                     (move-ant ant (random-elt best-directions)))
                   (>= best-score 0s0))))))))

(defun ant-danger (row col)
  (let ((danger 0))
    (dolist (dir '(nil :north :south :east :west))
      (multiple-value-bind (nr nc)
          (new-location row col dir)
        (when (and (accessiblep nr nc)
                   (forbidden-at nr nc))
          (incf danger))))
    danger))

(defun ant-compare (ant-1 ant-2)
  (let ((danger-1 (first ant-1))
        (danger-2 (first ant-2)))
    (cond ((> danger-1 danger-2) t)
          ((< danger-1 danger-2) nil)
          (t (let ((mobility-1 (second ant-1))
                   (mobility-2 (second ant-2)))
               (cond ((> mobility-1 mobility-2) t)
                     (t nil)))))))

;; This is the actual 'AI'.
(defun do-turn ()
  #+extra-logging (log-line "ants list " (ants *state*))
  (catch 'finish-turn
    (let ((sorted-ants (timed (-> (ants *state*)
                                  (mapcar (lambda (ant)
                                            (list* (ant-danger (first ant) (second ant))
                                                   (ant-mobility (first ant) (second ant))
                                                   ant))
                                          $)
                                  (sort $ #'ant-compare)))))
      (dolist (cluster (cluster-moves *state*))
        (dolist (ant-coords-dir cluster)
          (destructuring-bind ((row col owner) dir) ant-coords-dir
            (declare (ignore owner))
            (let ((ant (get-ant-at row col)))
              (move-ant ant dir)
              (setf (is-processed ant) t)))))
      (let (retry-list)
        (dolist (ant sorted-ants)
          (unless (handle-ant (third ant) (fourth ant))
            (push ant retry-list)))
        (dolist (ant (nreverse retry-list))
          (handle-ant (third ant) (fourth ant)))))))

(defun main (*standard-input* *standard-output*)
  "Main game loop: parses the (initial) game state and calls DO-TURN and
  FINISH-TURN."
  (setf *log* nil)
  (reset-random-state)
  #+visual-logging (img-start-thread)
  (handler-bind ((sb-sys:interactive-interrupt #'user-interrupt))
    (loop while (handler-case (peek-char nil *standard-input* nil)
                  (sb-int:simple-stream-error nil))
       for end-of-game-p = (parse-game-state)
       when end-of-game-p do (loop-finish)
       do
         (when (turn *state*)
           (do-turn))
         (finish-turn)))
  (log-line "finished successfully.")
  #+visual-logging (img-put-queue :quit)
  #-aichallenge (dump-and-reset-log))

(defun main-contest ()
  (main *standard-input* *standard-output*))

(defun host2str (host)
  (cond ((and (vectorp host) (= 4 (length host)))
         (format nil "~D.~D.~D.~D" (elt host 0) (elt host 1) (elt host 2)
                 (elt host 3)))
        (t host)))

(defvar *client*)
(defvar *server*)

#-aichallenge (defun clean-up ()
                (ignore-errors (usocket:socket-close *client*))
                (ignore-errors (usocket:socket-close *server*)))

#-aichallenge (defun main-proxy ()
                (clean-up)
                (let ((host "localhost")
                      (port 41807))
                  (let (stream)
                    (unwind-protect
                         (setf *server* (usocket:socket-listen host port :reuse-address t))
                      (format *debug-io* "Waiting for connection on ~A:~D...~%"
                              (host2str host)  port)
                      (force-output)
                      (setf *client* (usocket:socket-accept *server*)
                            stream (usocket:socket-stream *client*))
                      (format *debug-io* "Connected. Playing game...~%")
                      (force-output)
                      (main stream stream)
                      (ignore-errors (usocket:socket-close *client*)
                                     (usocket:socket-close *server*)))))
                (format *debug-io* "Game finished. Connection closed...~%"))

(let ((log nil))
  (defun close-log ()
    (when log
      (force-output log)
      (close log)))

  (defun logmsg (&rest args)
    (when log
      (format log (with-output-to-string (s) (dolist (a args) (princ a s))))
      (force-output log)))

  (defun open-log (&optional (file "proxy-bot.log"))
    (setf log (open file :direction :output :if-exists :append
                    :if-does-not-exist :create))))

(defparameter *host* "localhost")
(defparameter *port* 41807)

#-aichallenge (defun proxy ()
                (open-log)
                (let (socket)
                  (logmsg "~&=== ProxyBot started. ===~%")
                  (logmsg "Connecting to real bot at " (host2str *host*) ":" *port* "...~%")
                  (unwind-protect
                       (setf socket (usocket:socket-connect *host* *port*))
                    (loop with end-of-game-p = nil
                       with stream = (usocket:socket-stream socket)
                       while (peek-char nil *standard-input* nil)  ; run until EOF
                       for turn from 0
                       do (logmsg "--- turn: " turn " ---~%")
                         (logmsg "Sending game state...~%")
                         (loop for line = (read-line *standard-input* nil)
                            until (or (starts-with line "go")
                                      (starts-with line "ready"))
                            do (when line
                                 (logmsg "| " line "~%")
                                 (when (starts-with line "end")
                                   (setf end-of-game-p t))
                                 (write-line line stream)
                                 (force-output stream))
                            finally (when line
                                      (logmsg "| " line "~%")
                                      (write-line line stream)
                                      (force-output stream)))
                         (logmsg "Receiving bot response...~%")
                         (loop for line = (read-line stream nil)
                            until (or (starts-with line "go")
                                      end-of-game-p)
                            do (when line
                                 (logmsg "| " line "~%")
                                 (write-line line *standard-output*)
                                 (force-output *standard-output*))
                            finally (when line
                                      (logmsg "| " line "~%")
                                      (write-line line *standard-output*)
                                      (force-output *standard-output*)))
                         (when end-of-game-p
                           (loop-finish)))
                    (ignore-errors (usocket:socket-close socket))))
                ;; It doesn't get here if no "end" is sent by the game engine.
                (logmsg "~&=== ProxyBot finished. ===~%")
                (close-log))

#-aichallenge (setf sb-ext:*after-gc-hooks*
                    (list (lambda ()
                            (log-line "oops, garbage collection triggered"))))
