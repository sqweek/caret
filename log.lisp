;; ========================== Logging
(defvar *log-stream* *standard-output*)
(defparameter *log-file* "/home/sqweek/log/caret.log")

(defun caret-log (&rest args)
  (multiple-value-bind (ss mm hh day mon year) (decode-universal-time (get-universal-time) 0)
    (format *log-stream* "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d ~A~%"
            year mon day hh mm ss (apply #'format (cons nil args)))
    (force-output *log-stream*))
  nil)

(defun log-init ()
  (handler-case
    (setf *log-stream* (open *log-file* :direction :output
                             :if-exists :append
                             :if-does-not-exist :create
                             :external-format :utf-8))
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
