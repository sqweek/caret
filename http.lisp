;(require 'asdf)
;(setf asdf:*central-registry*
;      '(*default-pathname-defaults*
;         #p"/home/sqweek/lisp/"))
;
;(asdf:operate 'asdf:load-op 'trivial-http)
;(asdf:operate 'asdf:load-op 'cl-ppcre)

;(load "settings.lisp")

(defun get-cookie (username password)
  (destructuring-bind (response headers stream)
    (thttp:http-post (concatenate 'string "http://" *cmc-root* "CMClogin.jsp")
                     "application/x-www-form-urlencoded"
                     (concatenate 'string "username="
                                  (thttp:escape-url-query username)
                                  "&password="
                                  (thttp:escape-url-query password)))
    (if (eq response 200)
      (let ((data (make-array (parse-integer (cdr (assoc ':CONTENT-LENGTH headers))) :element-type 'character)))
        (read-sequence data stream)
        (unless (null (search "Success!" data))
          (let ((cookie (cdr (assoc ':SET-COOKIE headers))))
            (subseq cookie 0 (position #\; cookie)))))
      (nil))))

(defun get-code (cookie)
  (destructuring-bind (response headers stream)
    (thttp:http-get (concatenate 'string "http://" *cmc-root* "page.jsp?pageurl=match.jsp")
                    :headers `(("Cookie" . ,cookie)))
    (declare (ignore headers))
    (if (eq response 200)
      (let* ((data (make-array 32768 :element-type 'character)))
        (read-sequence data stream)
        (let* ((applet (cl-ppcre:scan-to-strings "(?is)<applet.*?>" data))
               (check-line (cl-ppcre:scan-to-strings "(?i)check \= \".*?\"" applet)))
          (subseq check-line
                  (+ 1 (position #\" check-line))
                  (position #\" check-line :from-end t))))
      (nil))))

