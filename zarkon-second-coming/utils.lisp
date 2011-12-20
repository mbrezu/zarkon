
#-aichallenge (declaim (optimize (debug 3) (speed 0) (safety 3)))
#+aichallenge (declaim (optimize (debug 0) (speed 1) (safety 0)))

#-(and aichallenge quicklisp)
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

#-aichallenge (ql:quickload :trivial-backtrace)
#-aichallenge (ql:quickload :usocket)
#-aichallenge (ql:quickload :cl-cairo2)
#-aichallenge (ql:quickload :bordeaux-threads)

(defvar *log-file-name* "log.txt")

(defvar *log*)
(defvar *log-count*)

(defun reset-log ()
  (setf *log* nil)
  (setf *log-count* 0))

(reset-log)

#-aichallenge
(defun dump-and-reset-log ()
  (with-open-file (file *log-file-name*
                        :direction :output
                        :if-exists :append
                        :if-does-not-exist :create)
    (let ((*print-pretty* nil))
      (dolist (line (nreverse *log*))
        (write-string (with-output-to-string (s) (dolist (a line) (princ a s)))
                      file)
        (terpri file))))
  (reset-log))

#+(and text-logging (not aichallenge))
(defun log-line (&rest args)
  (push args *log*)
  (incf *log-count*)
  (when (> *log-count* 100)
    (dump-and-reset-log)))

#+(or aichallenge (not text-logging))
(defmacro log-line (&rest args)
  (declare (ignore args))
  nil)

(defmacro -> (obj &rest forms)
  "Similar to the -> macro from clojure, but with a tweak: if there is
  a $ symbol somewhere in the form, the object is not added as the
  first argument to the form, but instead replaces the $ symbol."
  (if forms
      (if (consp (car forms))
          (let* ((first-form (first forms))
                 (other-forms (rest forms))
                 (pos (position '$ first-form)))
            (if pos
                `(-> ,(append (subseq first-form 0 pos)
                              (list obj)
                              (subseq first-form (1+ pos)))
                     ,@other-forms)
                `(-> ,(list* (first first-form) obj (rest first-form))
                     ,@other-forms)))
          `(-> ,(list (car forms) obj)
               ,@(cdr forms)))
      obj))

(defun random-elt (seq)
  (elt seq (random (length seq))))

(defun add-queue (queue element)
  (nconc queue (list element)))

(defun add-queue-many (queue elements)
  (nconc queue elements))

(defun consume-queue (queue)
  (pop queue)
  queue)

(defun queue-next (queue)
  (first queue))

(defun queue-empty (queue)
  (null queue))

(defmacro aif (test then &optional (else nil))
  `(let ((it ,test))
     (if it ,then ,else)))

(defmacro awhen (test &body body)
  `(let ((it ,test))
     (when it ,@body)))

(defmacro bif ((var test) then &optional (else nil))
  `(let ((,var ,test))
     (if ,var ,then ,else)))

(defmacro bwhen ((var test) &body body)
  `(let ((,var ,test))
     (when ,var ,@body)))

(defun reset-random-state ()
  (log-line "resetting random state")
  (setf *random-state*
        (read-from-string
         "#S(RANDOM-STATE :STATE #.(MAKE-ARRAY 627 :ELEMENT-TYPE '(UNSIGNED-BYTE 32)
                                             :INITIAL-CONTENTS
                                             '(0 2567483615 4 2601187879 3919438689
                                               2270374771 3254473187 705526435
                                               752899028 4259895275 1635503293
                                               287311810 3348146311 587101971
                                               1133963260 197444494 1569747226
                                               2853653046 3654449492 3823320007
                                               1939491435 191871982 2550916200
                                               2586577334 1836795533 2550787344
                                               3774101499 499856526 4035163043
                                               969324510 502882529 3747915135
                                               3677962142 4247339488 668043123
                                               3114378363 585508492 542098765
                                               4155704470 3660917119 126230230
                                               1522675206 153049183 1637257449
                                               3281868928 979462891 1058287769 61525060
                                               3887730846 3905100104 1956723994
                                               4085220955 3202629445 4112745420
                                               877772572 3341645661 331737137
                                               2270305335 159419296 2503600762
                                               451751822 1083485811 1445113017
                                               150608331 422828708 2507709208
                                               3804045526 2086909439 4090458745
                                               1662894632 2371871123 3266028938
                                               3412874573 1564584271 4167008239
                                               319160793 1225298391 4161956981
                                               3585990452 2573113091 1630884103
                                               997591457 996494329 2686829642
                                               3651385850 300660150 251233608
                                               2731448254 3271745554 1133669415
                                               2602763402 3609598910 3119890150
                                               2068448659 2512941629 2192028056
                                               3536672045 1605585867 987929371
                                               3628305377 3597455410 3551228490
                                               4133937922 2490571536 3920924840
                                               3569243872 3272512015 1796837098
                                               1735422750 3686651765 3897135636
                                               1148857629 783899959 2380757210
                                               1621036080 2512995685 1314946383
                                               195879552 1712690214 134121293
                                               2558246413 1909295793 4011814961
                                               3584678033 3552917322 2895522131
                                               2197479543 599948941 3054641120
                                               1896938373 3039964013 1872754348
                                               3148850798 2107216694 1457074081
                                               2885919429 2673983641 3439943002
                                               870488328 3207188349 2106353973
                                               3677712338 858424175 1253591073
                                               2543671298 2173076892 4241262707
                                               3125777792 937639453 3794644771
                                               1031184467 1762096386 1582522796
                                               2401451448 1923687658 3041175264
                                               1553520828 1499754517 3874023455
                                               2605056296 3039888102 1303021756
                                               1642751373 1823333037 1633927268
                                               3515841308 3444633309 3781936666
                                               2809133915 3605157463 2111775741
                                               4189224133 1541376461 2404717080
                                               4269480857 977189618 1665607742
                                               1629250805 2457077617 239411934
                                               3667589811 1869810277 1786963907
                                               2464100961 2087821940 1259003512
                                               276996953 797806588 3071924361
                                               2277580686 4202707827 1388129925
                                               2102369776 1628022127 2487208107
                                               3087861794 1052589112 419878642
                                               130232609 109631040 2052113021
                                               2352638891 3505327656 1245960092
                                               1830675294 3601635339 3493669978
                                               2998128382 1669215416 4294454851
                                               2093225537 1094833212 3273688535
                                               3217483090 2119564735 1480278974
                                               2437451062 1653704104 3486785552
                                               87986508 429079035 3325830834 1173159674
                                               3117122078 58610491 861618023 3127089575
                                               788511071 3165761011 3504889677
                                               1407514791 2912129370 2669842377
                                               65597446 2809811316 1936888672
                                               2735813429 2992168722 2213185349
                                               2759840193 3953154784 1561769082
                                               2042753077 1431122408 2948426693
                                               222113656 1532323091 3465951077
                                               531543838 463855516 259721548 595116526
                                               3129247900 2801040682 538274173
                                               3291042483 3080595328 1887205203
                                               158727281 3040111581 218801154
                                               3984624257 2054350644 2080133350
                                               637187864 2040737093 616990199
                                               3893926523 79368930 2444288761
                                               2454929062 816464100 1797157122
                                               2590520370 1517945845 585466041
                                               2048644740 1148006305 232096794
                                               1288445919 2815085715 3360592226
                                               2612544241 3735001635 1982293843
                                               2437762859 1380875628 1511870631
                                               1503310543 3124191302 672465147 1097623
                                               2986124200 3488182882 3458056040
                                               960516857 2873195180 4198947675 21992134
                                               3918247947 3659648629 3966894158
                                               1603926895 3947065708 4064432254
                                               314205538 872618379 3563243059 304253681
                                               783558693 2503271980 3495646321
                                               1290872763 3121520939 2942098806
                                               641090084 2040744877 2641640520
                                               2822298419 2885259028 1013302765
                                               2006133592 3672833023 266147116
                                               2422267738 501894401 345672645
                                               2384900549 3593315420 168531307
                                               3138115426 2559469854 878370741
                                               567096748 1342975608 1929917421
                                               3815079155 3887969580 379967032
                                               3933320244 1022246178 3298764003
                                               3352606638 3810828369 1116729323
                                               2381652356 622561456 2745681725
                                               3387916065 938595997 4901396 3264727977
                                               3990265482 1545695311 2583073239
                                               25719318 359879420 2690926668 1737962267
                                               1872326961 2971436963 856135260
                                               1262752712 1803801525 3825067982
                                               227806404 531264838 2692088805
                                               1020958178 2462141362 867482822
                                               4142146091 149020123 3976685847
                                               2156258478 1566485989 636188758
                                               1911328438 1228372759 761095935 93755721
                                               1443003772 937784737 2078374011
                                               4179063358 1256093635 2390763207
                                               1312271775 498684952 3381068839
                                               2094766662 3384348866 4250970780
                                               2736167046 1975999640 3449310360
                                               1249986286 1623556171 2459105852
                                               1161613179 4241934306 3592740997
                                               4164507334 1031114739 2038402399
                                               945921397 405487343 138708438 3414161765
                                               2912469629 220667773 2009463834
                                               112293856 3463567532 4173647700
                                               2447477757 1074847995 2240777610
                                               3343165669 1207699502 2670340302
                                               1464222360 145352797 4013782448
                                               2798704117 3854604174 301538628
                                               3254669258 3496478697 195964083
                                               2340356226 158045975 89523493 3012065632
                                               1685882608 582880035 3084954840
                                               3758218654 2812719378 914681218
                                               3492165600 252202130 3133981835
                                               1682632314 1511367028 3091183057
                                               2169963046 1791825518 26880152 943542572
                                               124950977 3436569249 2931277172
                                               2360174007 2297876068 324140282
                                               3145103099 878367086 746517350
                                               2447571951 113826277 455271391
                                               2021629898 311130642 4190715446
                                               2096076314 2637293993 4081579075
                                               1195677250 2995667596 3378498797
                                               2310622554 4036366420 2255722665
                                               653284776 2734982087 776639191
                                               1840567454 2718554699 4121086132
                                               3648536374 376160211 345701825
                                               3172728420 1574538422 1459410665
                                               4197243794 4047198565 4156270602
                                               4158935841 3766821729 1411764188
                                               3632321377 3403794111 3833557234
                                               3599364771 1036284889 24670187
                                               2235700863 4190632977 425187274
                                               2031251706 3670054996 2155400770
                                               192611794 2839969695 299462972 12797895
                                               3966267574 3400571208 3669765080
                                               3508427657 3235589560 3193293610
                                               925737567 1488513860 2440222622
                                               1120038826 3957100300 1727699532
                                               2066529857 1405887448 3610495290
                                               958674275 405728232 3364823839 673899695
                                               3789105973 1675609960 98445821
                                               3209212267 298129863 330602789
                                               2765562200 507842865 729452063
                                               2371849561 2107993350 3060623393
                                               756469007 1494691292 289556860 91472947
                                               1249357945 854006002 2109176687
                                               1212277959 3811972867 3564054431
                                               671407310 118971092 1896128714 551706431
                                               2067080292 1335799548 3473466841
                                               1549134658 32104447 2995416126 544027868
                                               52897576 1642961318 2693801207
                                               3726909303 218117047 4156633525
                                               3536249408 1009174707 4181022899
                                               3945338608 354768839 2202138544
                                               215184568 61111279 1914789892 3049369045
                                               3404495672 3795455853 2610561194
                                               503121087 23446167 323357009 3710519877
                                               922256365 2131896758 744734225 970651722
                                               1335476026 1974620788 2256794949
                                               3015924989 4048571554 1990784236
                                               22896868 3992110738 2560474406
                                               4215514841 974260437 3208940655
                                               4167287399 2454179266 4093879596
                                               2462688439 163846102 4211244342
                                               2092190284 2985895911 1620740022
                                               2656641972 449410644 3973823023
                                               3960679300 1074759248 698903848
                                               1233675965 3236169158 415317533
                                               1028329372 2994946905 3311476626
                                               1172312971 3837858120 2085838740
                                               3576264772 2903063865 3505442042
                                               3518038711)))")))

#+(and (not aichallenge) text-logging)
(defmacro timed (&body body)
  (let ((g-start (gensym "start"))
        (g-result (gensym "result")))
    `(let ((,g-start (turn-time-remaining))
           (,g-result (progn
                        ,@body)))
       (log-line '(progn ,@body) " took " (- ,g-start (turn-time-remaining)))
       (log-line "remaining time " (turn-time-remaining))
       ,g-result)))

#+(or aichallenge (not text-logging))
(defmacro timed (&body body)
  `(progn
     ,@body))

#-aichallenge
(defvar *lock* (bt:make-lock))

#-aichallenge
(defvar *queue*)

#-aichallenge
(defun img-reset-queue ()
  (setf *queue* nil))

#-aichallenge
(defun img-put-queue (item)
  (bt:with-lock-held (*lock*)
    (setf *queue* (nconc *queue* (list item)))))

#-aichallenge
(defun img-pop-queue ()
  (bt:with-lock-held (*lock*)
    (pop *queue*)))

#-aichallenge
(defun img-queue-empty ()
  (bt:with-lock-held (*lock*)
    (null *queue*)))

(defvar *zoom* 8)

#-aichallenge
(defun draw-big-pixel (x y r g b a)
  (progn
    (cl-cairo2:set-source-rgba r g b a)
    (cl-cairo2:rectangle (* x *zoom*)
                         (* y *zoom*)
                         *zoom*
                         *zoom*)
    (cl-cairo2:fill-path)))

#-aichallenge
(defun draw-extra-big-pixel (x y r g b a)
  (progn
    (cl-cairo2:set-source-rgba r g b a)
    (cl-cairo2:rectangle (1-  (* x *zoom*))
                         (1- (* y *zoom*))
                         (+ 2 *zoom*)
                         (+ 2 *zoom*))
    (cl-cairo2:fill-path)))

#-aichallenge
(defun draw-rectangle (x y r g b a)
  (progn
    (cl-cairo2:set-source-rgba r g b a)
    (cl-cairo2:rectangle (1-  (* x *zoom*))
                         (1- (* y *zoom*))
                         (+ 2 *zoom*)
                         (+ 2 *zoom*))
    (cl-cairo2:stroke)))

#-aichallenge
(defun draw-text-at (col row text)
  (progn (cl-cairo2:move-to (* *zoom* col) (* *zoom* row))
         (cl-cairo2:set-source-rgba 0.0 0.0 0.0 1.0)
         (cl-cairo2:show-text text)))

#-aichallenge
(defun draw-line (x1 y1 x2 y2)
  (progn
    (let ((half-zoom (/ *zoom* 2)))
      (cl-cairo2:move-to (+ (* *zoom* x1) half-zoom)
                         (+ (* *zoom* y1) half-zoom))
      (cl-cairo2:line-to (+ (* *zoom* x2) half-zoom)
                         (+ (* *zoom* y2) half-zoom))
      (cl-cairo2:set-source-rgba 1.0 1.0 1.0 1.0)
      (cl-cairo2:stroke))))


#-aichallenge
(defun draw-image (description)
  (destructuring-bind (turn width height) (subseq description 0 3)
    (cl-cairo2:with-png-file ((format nil "images/turn-~4,'0d.png" turn)
                              :argb32 (* *zoom* width) (* *zoom* height))
      (cl-cairo2:select-font-face "Monospace" :normal :bold)
      (cl-cairo2:set-font-size 10)
      (dolist (instruction (nreverse (subseq description 3)))
        (ecase (first instruction)
          ((:draw-big-pixel) (apply #'draw-big-pixel (rest instruction)))
          ((:draw-extra-big-pixel) (apply #'draw-extra-big-pixel (rest instruction)))
          ((:draw-rectangle) (apply #'draw-rectangle (rest instruction)))
          ((:draw-text-at) (apply #'draw-text-at (rest instruction))))))))

#-aichallenge
(defun img-start-thread ()
  (setf *queue* (list :quit))
  (sleep 0.5)
  (setf *queue* nil)
  (bt:make-thread (lambda ()
                    (loop
                       (cond ((img-queue-empty)
                              (sleep 0.05))
                             (t (let ((popped (img-pop-queue)))
                                  (when (eq :quit popped)
                                    (return))
                                  (draw-image popped))))))))


(defun tl-make-empty ()
  (cons nil nil))

;; (defun tl-push-front (tl elm)
;;   (declare (optimize speed))
;;   (if (null (car tl))
;;       (let ((list-elm (list elm)))
;;         (cons list-elm list-elm))
;;       (progn
;;         (setf (car tl) (cons elm (car tl)))
;;         tl)))

(defun tl-push-back (tl elm)
  (declare (optimize speed))
  (if (null (car tl))
      (let ((list-elm (list elm)))
        (setf (car tl) list-elm)
        (setf (cdr tl) list-elm)
        tl)
      (progn
        (setf (cdr (cdr tl)) (list elm))
        (setf (cdr tl) (cdr (cdr tl)))
        tl)))

(defun tl-empty (tl)
  (declare (optimize speed))
  (and (null (car tl))
       (null (cdr tl))))

(defun tl-pop-front (tl)
  (declare (optimize speed))
  (let ((result (car (car tl))))
    (setf (car tl) (cdr (car tl)))
    (when (null (car tl))
      (setf (cdr tl) nil))
    result))
