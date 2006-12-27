(defvar *mm-name*)
(defvar *mm-login-code*)
(defvar *mm-playlist* nil)

(defvar *mm-modules* nil)

;TODO trades
;; hooks are plists of :module #'function pairs, ordered on a FCFS basis
(defvar *mm-init-hooks* nil) ;;started up
(defvar *mm-chat-hooks* nil) ;;someone said something
(defvar *mm-join-hooks* nil) ;;someone joined
(defvar *mm-play-hooks* nil) ;;people are playing
(defvar *mm-part-hooks* nil) ;;someone left
(defvar *mm-mode-hooks* nil) ;;someone changed mode

(defvar *mm-modes* '(play trade chat away))

;(defclass connection ()
;  ((name
;     :initarg :name
;     :accessor name)
;   (code
;     :initarg :code
;     :accessor code)
;   (stream)
;   (playlist)
;   (init-hooks)
;   (chat-hooks)
;   (join-hooks)
;   (part-hooks)))

;; Utility functions
(defun split (sep str)
  (unless (or (null str) (zerop (length str)))
    (let ((pos (position sep str)))
      (cond ((null pos) (cons str nil))
            (t (cons (subseq str 0 pos) (split sep (subseq str (+ 1 pos)))))))))

(defun join (sep seq)
  (unless (null seq)
    (concatenate 'string (car seq) sep (join sep (cdr seq)))))

;; Matchmaker stuff
(defclass pl-entry ()
  (name wins losses mode isdonor trophies))

; TODO parse flags to fill in trophies
(defun build-pl-entry (name wins losses mode flags)
  (let ((entry (make-instance 'pl-entry))
        (flag-list (split #\! flags)))
    (setf (slot-value entry 'name) name)
    (setf (slot-value entry 'wins) (parse-integer wins))
    (setf (slot-value entry 'losses) (parse-integer losses))
    (setf (slot-value entry 'mode) (elt *mm-modes* (parse-integer mode)))
    (setf (slot-value entry 'isdonor) (if (find "donor" flag-list :test #'equal) t nil))
    (setf (slot-value entry 'trophies) 0)
    entry))

(defun build-pl-list (args)
  (cond ((= (mod (length args) 5) 0)
         (let ((pl-entries nil))
           (dotimes (i (/ (length args) 5))
             (push (apply #'build-pl-entry (subseq args (* i 5) (* (+ i 1) 5)))
                   pl-entries))
           pl-entries))))

(defun mm-get-player (name)
  (find-if (lambda (entry) (equal name (slot-value entry 'name))) *mm-playlist*))

(defun parse-message (message)
  (let* ((handler (intern (concatenate 'string "HANDLE-" (car message) "-MSG"))))
    (if (fboundp handler)
      (apply handler (cdr message))
      (format t "No handler for message ~{~A#~}~%" message))))

(defun message-loop (stream)
  (do ()
    (nil)
    (let* ((message-line (read-line stream))
           (message (split #\# message-line))
           (response (multiple-value-list (parse-message message))))
        (progn (dolist (command response)
                 (when command
                   (write-line (join "#" command) stream))
                 (force-output stream))))))

(defun run-hooks (hooks &rest args)
  (let ((results nil))
    (loop while hooks do
          (multiple-value-bind (module hook tail)
            (get-properties hooks *mm-modules*)
            (when module (setq results (nconc results (multiple-value-list (apply hook args)))))
            (setf hooks (cddr tail))))
    (values-list results)))
  ;(let ((results nil))
    ;(dolist (hook hooks (values-list results))
      ;(setq results (nconc results (multiple-value-list (apply hook args)))))))

;TODO type safety
(defmacro add-hook (hooks module hook)
  `(if (getf ,hooks ,module)
     (setf (getf ,hooks ,module) ,hook)
     (setf ,hooks (nconc ,hooks (list ,module ,hook)))))

(defun add-join-hook (module hook)
  (add-hook *mm-join-hooks* module hook))
(defun add-part-hook (module hook)
  (add-hook *mm-part-hooks* module hook))
(defun add-chat-hook (module hook)
  (add-hook *mm-chat-hooks* module hook))
(defun add-init-hook (module hook)
  (add-hook *mm-init-hooks* module hook))
(defun add-play-hook (module hook)
  (add-hook *mm-play-hooks* module hook))
(defun add-mode-hook (module hook)
  (add-hook *mm-mode-hooks* module hook))

(defun enable-module (module)
  (setf *mm-modules* (adjoin module *mm-modules*)))

(defun disable-module (module)
  (setf *mm-modules* (set-difference *mm-modules* `(,module))))

(defun handle-rdy-msg ()
  (values-list (cons `("LOGIN" ,*mm-name* ,*mm-login-code* "1")
                     (multiple-value-list (run-hooks *mm-init-hooks*)))))

(defun handle-png-msg ()
  '("PN"))

(defun handle-chastat-msg (info)
  ; Joins/quits/modes are handled via playlist ftw, so just deal with matches
  (multiple-value-bind (match players)
    (cl-ppcre:scan-to-strings "(.*) and (.*) are playing Cardmaster Conflict!" info)
    (declare (ignore match))
    (if players
      (run-hooks *mm-play-hooks*
                 (mm-get-player (elt players 0))
                 (mm-get-player (elt players 1))))))

(defun handle-chat-msg (player text)
  (run-hooks *mm-chat-hooks* (mm-get-player player) text))

;; wow holy crap, could use some abstraction here
(defun handle-playlist-msg (&rest args)
  (let ((pl-entries (build-pl-list args)))
    (unless (null pl-entries)
      (flet ((k (x) (slot-value x 'name)))
        (let ((joins (set-difference pl-entries *mm-playlist*
                                     :key #'k :test 'equal))
              (parts (set-difference *mm-playlist* pl-entries
                                     :key #'k :test 'equal))
              (results nil))
          (dolist (player parts)
            (setq results (nconc results (multiple-value-list
                                           (run-hooks *mm-part-hooks* player)))))
          (dolist (player joins)
            (setq results (nconc results (multiple-value-list
                                           (run-hooks *mm-join-hooks* player)))))
          ; TODO mode changes
          ;(dolist (player stays)

          (setq *mm-playlist* pl-entries)
          (values-list results))))))
