;; ========================== Logging
(defvar *log-stream* *standard-output*)
(defparameter *log-file* "/home/sqweek/log/caret.log")

(defun caret-log (&rest args)
  (multiple-value-bind (ss mm hh day mon year) (decode-universal-time (get-universal-time) 0)
    (format *log-stream* "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d ~A~%"
            year mon day hh mm ss (apply #'format (cons nil args)))
    (force-output *log-stream*))
  nil)

(defun caret-chat (&rest args)
  (let ((msg (apply #'format (cons nil args))))
    ;;(caret-log "<~A> ~A" *name* msg)
    `("CHAT" ,msg)))

(defun log-init ()
  (handler-case
    (setf *log-stream* (open *log-file* :direction :output
                             :if-exists :append
                             :if-does-not-exist :create))
    (error () (setf *log-stream* *standard-output*)
           (format t "Error opening ~A~%" *log-file*)))
  (caret-log "Connected."))

(add-init-hook :log #'log-init)
(add-join-hook :log (lambda (p) (with-slots (name) p (caret-log "--> ~A joins" name))))
(add-mode-hook :log (lambda (p) (with-slots (name mode) p (caret-log "=== ~A changes to mode ~S" name mode))))
(add-chat-hook :log (lambda (p msg) (with-slots (name) p (caret-log "<~A> ~A" name msg))))
(add-play-hook :log (lambda (p1 p2) (caret-log "!!! ~A is fighting ~A" (slot-value p1 'name) (slot-value p2 'name))))
(add-part-hook :log (lambda (p) (with-slots (name) p (caret-log "<-- ~A quits" name))))

(enable-module :log)


;; ========================== Backup
(defparameter *backup-time* (get-universal-time))
(defparameter *backup-interval* 3600)
(defparameter *backup-core* "cmcbot.core")

(defun backup (&rest args)
  (declare (ignore args))
  (when (> (- (get-universal-time) *backup-time*) *backup-interval*)
    (setf *backup-time* (get-universal-time))
    (when (= (sb-posix:fork) 0)
      (sb-ext:save-lisp-and-die *backup-core*))))

(add-init-hook :backup #'backup)
(add-join-hook :backup #'backup)
(add-mode-hook :backup #'backup)
(add-chat-hook :backup #'backup)
(add-play-hook :backup #'backup)
(add-part-hook :backup #'backup)

(enable-module :backup)


;; ========================== General
(defun startup ()
  (values '("CHAT" "/chat")))

(defun english-list (list &key (comb "or"))
  (if (cdr list)
    (if (cadr list)
      (format nil "~A, ~A" (car list) (english-list (cdr list)))
      (format nil "~A ~A ~A" (car list) comb (cdr list)))
    (format nil "~A" (car list))))

(defun random-word (&rest list)
  (elt list (random (length list))))

(defun caret-cmd (pl-entry msg)
  (cl-ppcre:register-groups-bind
    (cmd args)
    ("^\\^(\\w*) ?(.*)" msg)
    (when cmd
      (let ((cmd-fun (intern (concatenate 'string "CARET-CMD-" (string-upcase cmd)))))
        (if (fboundp cmd-fun)
          (apply cmd-fun (list pl-entry args))
          (format t "No command found: ~S~%" cmd-fun))))))

(defun caret-cmd-reload (pl-entry args)
  (declare (ignore args))
  (when (string= (slot-value pl-entry 'name) "sqweek")
    (require 'cmcbot)))

(defun caret-cmd-help (pl-entry args)
  "^help <command> : get help on commands where available"
  (declare (ignore pl-entry))
  (let ((cmd-fun (intern (concatenate 'string "CARET-CMD-" (string-upcase args)))))
    (if (fboundp cmd-fun)
      (caret-chat "~A: ~A" args (or (documentation cmd-fun 'function)
                                    "no help available"))
      (caret-chat "~A: no such command" args))))

(defun caret-cmd-info (pl-entry args)
  (declare (ignore pl-entry args))
  (caret-chat "Caret-0.7.2 written by sqweek in Lisp"))

(add-init-hook :misc #'startup)
(add-chat-hook :misc #'caret-cmd)

;TODO automatically reconnect when things go sideways
(defun caret-go ()
  (setf *mm-name* "Caret")
  (setf *mm-login-code* (get-code (get-cookie *name* *cmc-pass*)))
  (with-open-stream
    (stream (trivial-sockets:open-stream *cmc-host* *cmc-mmpt*))
    (message-loop stream))
  (caret-log "Disconnected."))

(enable-module :misc)

;; ========================== Fun
(defun caret-cmd-boggle (pl-entry msg)
  (declare (ignore pl-entry msg))
  (caret-chat "Ha, Gorilla, right there!"))

(defun caret-cmd-dice (pl-entry msg)
  (with-slots (name) pl-entry
    (handler-case
      (caret-chat "~A: ~A = ~D" name msg (eval (with-input-from-string (s msg) (dice-expr s))))
      (parse-error () (caret-chat "~A: ~A = Syntax Error" name msg)))))

(defun caret-cmd-eggplant (pl-entry msg)
  (declare (ignore pl-entry msg))
  (caret-chat "Meal of the moment: eggplant ~A" (random-word "foccacia" "quiche" "omelette" "bolognese" "pasta" "lasagne" "parmesan" "parmelia" "parmigiana" "lemonade" "soup" "sandwiches" "burgers" "sushi" "voulavants" "jaffles" "waffles")))

;; ========================== Time
(defvar *players-time* (make-hash-table :test #'equal))

(defun update-hist (player &optional (update-time nil))
  (let* ((cur-utime (or update-time (get-universal-time)))
         (old-utime (lasthist player)))
    (multiple-value-bind (old-sec old-min old-hr)
      (decode-universal-time old-utime 0)
      (let ((secs-left (- cur-utime old-utime))
            (secs-hr1 (- 3600 (* old-min 60) old-sec)))
        (incf (elt (timehist player) old-hr) (/ secs-hr1 3600))
        (decf secs-left secs-hr1)
        (do ((hr (+ old-hr 1) (setf hr (mod (+ 1 hr) 24))))
          ((> 3600 secs-left)
           (incf (elt (timehist player) hr) (/ secs-left 3600)))
          (incf (elt (timehist player) hr))
          (decf secs-left 3600))))
    (setf (slot-value player 'lasthist) nil)))

(defun (setf lasthist) (time player)
  (when (slot-value player 'lasthist) (update-hist player))
  (setf (slot-value player 'lasthist) time))

(defclass player-time ()
  ((name :accessor name
         :initarg :name
         :initform (error "New player needs name badly!"))
  (timezone :accessor timezone
            :initform nil
            :documentation "local-time:timezone")
  (seen :accessor seen
        :documentation
        "what the player was last seen doing, a list of the form:
        (TIMESTAMP ACTIVITY ARG)
        TIMESTAMP is a universal timestamp
        ACTIVITY is one of 'chat 'play 'join
        ARG is a string for 'chat, name of opponent for 'play and nil for 'join")
  (timehist :accessor timehist ;; TODO
            :initform (make-array 24)
            :documentation "distribution of time spent online (GMT)")
  (lasthist :reader lasthist
            :initform nil
            :documentation "universal timestamp of when histogram was last updated")
  ))

(defparameter *tz-list*
  (with-open-file (stream "/www/cmc/timezone-list.txt")
    (let ((tz-list nil))
      (do ((tz (read-line stream nil) (read-line stream nil))) ((not tz) tz-list)
        (push tz tz-list)))))

(defvar +utc-timezone+ nil)
(local-time:define-timezone +utc-timezone+ "/usr/share/zoneinfo/UTC" :load t)

(defun local-time-adjust (source timezone)
  (let ((adjusted-time (local-time:now)))
    (local-time:local-time-adjust source timezone adjusted-time)))

(defvar +omfg-lol-temp-tz+ nil)
(defun local-time-tz (tz-name)
  (handler-case
    (progn (local-time:define-timezone
             +omfg-lol-temp-tz+
             (concatenate 'string "/usr/share/zoneinfo/" tz-name)
             :load t)
           +omfg-lol-temp-tz+)
    (simple-error () nil)
    (sb-int:simple-file-error () nil)
    (sb-int:simple-stream-error () nil)))

(defun tz-abbrev (tz)
  (elt (multiple-value-list (local-time:timezone (local-time:now) tz)) 2))

(defun time-str (localtime tz)
  (multiple-value-bind (msec sec min hr date mon yr)
    (local-time:decode-local-time localtime)
    (declare (ignore msec date mon yr))
    (format nil "~2,'0d:~2,'0d:~2,'0d ~A" hr min sec (tz-abbrev tz))))

(defun duration-str (duration)
  (if duration
    (with-output-to-string (str)
      (format str "~D ~A~2:*~P" (caar duration) (cadar duration))
      (do ((sub (cdr duration) (setf sub (cdr sub)))) ((null sub))
        (if (cdr sub)
          (format str ", ~D ~A~2:*~P" (caar sub) (cadar sub))
          (format str " and ~D ~A~2:*~P" (caar sub) (cadar sub)))))
    "0 seconds"))

(defun time-since (anchor-time)
  (let ((diff (- (get-universal-time) anchor-time))
        (duration '((0 "day" 86400) (0 "hour" 3600)
                                    (0 "minute" 60) (0 "second" 1))))
    (dolist (sub duration (mapcar #'butlast
                                  (remove-if (lambda (period) (= (car period) 0))
                                             duration)))
      (setf (car sub) (floor diff (caddr sub)))
      (decf diff (* (car sub) (caddr sub))))))

;TODO factor out duplicate matching code
(defun caret-cmd-timezone (pl-entry args)
  "Specify your timezone. See http://sqweek.dnsdojo.org/cmc/timezone-list.txt for a full listing"
  (with-slots (name) pl-entry
    (let ((p (gethash (string-downcase name) *players-time*))
          (matches (remove-if-not (lambda (x)
                                    (search args x :test #'string-equal))
                                  *tz-list*)))
      (when p
        (case (length matches)
          (0 (caret-chat "Sorry ~A, I don't know any timezones that look like ~A" name args))
          (1 (let ((tz (local-time-tz (car matches))))
               (setf (timezone p) tz)
               (caret-chat "Noted: ~A's timezone is ~A (~A)" name (tz-abbrev tz) (car matches))))
          ((2 3 4 5 6 7 8 9) (caret-chat "~A, did you perhaps mean one of ~A?" name (english-list matches)))
          (otherwise (caret-chat "~A is way too vague a timezone ~A, I have ~D matches!"
                                 args name (length matches))))))))

(defun caret-cmd-time (pl-entry args)
  (declare (ignore pl-entry))
  (flet ((curtime (tz)
                  (caret-chat "It is currently ~A"
                              (time-str (local-time-adjust (local-time:now) tz)
                                        tz))))
    (if (< 0 (length args))
      (let ((p (gethash (string-downcase args) *players-time*))
            (tz (local-time-tz args)))
        (if p
          (if (timezone p)
            (curtime (timezone p))
            (caret-chat "~A hasn't told me their timezone :(" args))
          (if tz
            (curtime tz)
            (let ((matches (remove-if-not (lambda (x)
                                            (search args x :test #'string-equal))
                                          *tz-list*)))
              (case (length matches)
                (0 (caret-chat "I don't know any player or timezone by that name"))
                (1 (curtime (local-time-tz (car matches))))
                ((2 3 4 5 6 7 8 9) (caret-chat "Near matches: ~A" (english-list matches :comb "and")))
                (otherwise (caret-chat "~D timezones match \"~A\", be more specific"
                                       (length matches) args)))))))
      (curtime +utc-timezone+))))

(defun caret-cmd-seen (pl-entry name)
  (declare (ignore pl-entry))
  (let ((p (gethash (string-downcase name) *players-time*)))
    (if p
      (destructuring-bind (time activity arg) (seen p)
        (let ((dur-str (duration-str (time-since time))))
          (case activity
            ('play (caret-chat "~A was seen ~A ago, playing ~A"
                               name dur-str arg))
            ('chat (caret-chat "~A was seen ~A ago, saying: ~A"
                               name dur-str arg))
            ('join (caret-chat "~A was seen ~A ago, joining the matchmaker"
                               name dur-str))
            (otherwise (caret-chat "~A was seen ~A ago, but I'm not sure what they were doing" name dur-str)))))
      (caret-chat "I have never seen ~A!" name))))

(defun tm-join (pl-entry)
  (with-slots (name) pl-entry
    (if (not (gethash (string-downcase name) *players-time*))
      (setf (gethash (string-downcase name) *players-time*) (make-instance 'player-time :name name)))
    (let ((p (gethash (string-downcase name) *players-time*))
          (time (get-universal-time)))
      (setf (seen p) `(,time join nil)
            (lasthist p) time)))
  nil)

(defun tm-quit (pl-entry)
  (with-slots (name) pl-entry
    (update-hist (gethash (string-downcase name) *players-time*))))

(defun tm-chat (pl-entry msg)
  (with-slots (name) pl-entry
    (let ((p (gethash (string-downcase name) *players-time*)))
      (with-slots (seen) p
        (setf seen (list (get-universal-time) 'chat msg))
        nil))))

(defun tm-play (pl1 pl2)
  (let ((p1 (gethash (string-downcase (slot-value pl1 'name)) *players-time*))
        (p2 (gethash (string-downcase (slot-value pl2 'name)) *players-time*))
            (time (get-universal-time)))
    (when (and p1 p2)
      (setf (seen p1) (list time 'play (slot-value pl2 'name)))
      (setf (seen p2) (list time 'play (slot-value pl1 'name)))
      nil)))

(add-join-hook :time #'tm-join)
(add-chat-hook :time #'tm-chat)
(add-part-hook :time #'tm-quit)
(add-play-hook :time #'tm-play)

(enable-module :time)
