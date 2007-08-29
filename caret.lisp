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

(defun caret-chat (&rest args)
  (let ((msg (apply #'format (cons nil args))))
    `("CHAT" ,msg)))

(defun caret-cmd (pl-entry msg)
  (when (not (string= (slot-value pl-entry 'name) "Caret"))
    (cl-ppcre:register-groups-bind
      (cmd args)
      ("^\\^(\\w*) ?(.*?) *$" msg)
      (when cmd
        (let ((cmd-fun (intern (concatenate 'string "CARET-CMD-" (string-upcase cmd)))))
          (if (fboundp cmd-fun)
            (apply cmd-fun (list pl-entry args))
            (format t "No command found: ~S~%" cmd-fun)))))))

(defun caret-cmd-reload (pl-entry args)
  (declare (ignore args))
  (when (string= (slot-value pl-entry 'name) "sqweek")
    (require 'cmcbot)))

(defun caret-cmd-help (pl-entry args)
  "^help <command> : get help on commands where available"
  (declare (ignore pl-entry))
  (if (< 0 (length args))
    (let ((cmd-fun (intern (concatenate 'string "CARET-CMD-" (string-upcase args)))))
      (if (fboundp cmd-fun)
        (caret-chat "~A: ~A" args (or (documentation cmd-fun 'function)
                                      "no help available"))
        (caret-chat "~A: no such command" args)))
    (caret-chat "available commands: dice info quote seen time timezone")))

(defun caret-cmd-info (pl-entry args)
  (declare (ignore pl-entry args))
  (caret-chat "Caret-0.9.3 written by sqweek in Lisp"))

(add-init-hook :misc #'startup)
(add-chat-hook :misc #'caret-cmd)

(enable-module :misc)

(defun getaddr (hostname)
  (car (sb-bsd-sockets:host-ent-addresses
         (sb-bsd-sockets:get-host-by-name hostname))))

(defun connect (hostname port)
  (let ((s (make-instance 'sb-bsd-sockets:inet-socket
                          :type :stream
                          :protocol :tcp))
        (addr (getaddr hostname)))
    (handler-case
      (progn
        (sb-bsd-sockets:socket-connect s addr port)
        (sb-bsd-sockets:socket-make-stream s :input t :output t
                                           :element-type 'character
                                           :buffering :full))
      (sb-bsd-sockets:socket-error
        (condition)
        (when (sb-bsd-sockets:socket-open-p s)
          (sb-bsd-sockets:socket-close s))
        (error condition)))))

(defun caret-go ()
  (setf *mm-name* "Caret")
  (tagbody retry
           (handler-case
             (progn
               (setf *mm-login-code* (get-code (get-cookie *name* *cmc-pass*)))
               (with-open-stream
                 (stream (connect *cmc-host* *cmc-mmpt*))
                 (message-loop stream)))
             (timeout ()
                      (go retry))
             (sb-bsd-sockets:socket-error ()
               (format t "Connect failed, retrying in 2 minutes~%")
               (sleep 120)
               (go retry)))))
