;; ========================== Backup
(defparameter *backup-time* (get-universal-time))
(defparameter *backup-interval* 3600)
(defparameter *backup-file* "caret-data.lisp")

(defun force-backup (file)
  (with-open-file (stream file :direction :output
                          :if-exists :supersede)
    (dolist (persist-fn (apropos-list "CARET-PERSIST"))
      (when (fboundp persist-fn)
        (funcall persist-fn stream))))
  nil)

(defun backup (&rest args)
  (declare (ignore args))
  (when (> (- (get-universal-time) *backup-time*) *backup-interval*)
    (setf *backup-time* (get-universal-time))
    (force-backup *backup-file*)
    nil))

(add-init-hook :backup #'backup)
(add-join-hook :backup #'backup)
(add-mode-hook :backup #'backup)
(add-chat-hook :backup #'backup)
(add-play-hook :backup #'backup)
(add-part-hook :backup #'backup)

(defun caret-cmd-backup (pl-entry args)
  (when (string= (slot-value pl-entry 'name) "sqweek")
    (force-backup (or args *backup-file*))))

(enable-module :backup)
