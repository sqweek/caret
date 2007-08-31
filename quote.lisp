(defvar *quote-list* (make-array 1 :fill-pointer 0 :adjustable t))
(defparameter *quote-mods* '("sqweek" "Leander"))

(defun caret-cmd-quote (pl-entry args)
  "^quote <number> for a specific quote, ^quote count gives the number of quotes, ^quote add <quote> to add a quote."
  (cond ((= 0 (length args))
         (if (< 0 (length *quote-list*))
           (let ((num (random (length *quote-list*))))
             (caret-chat "~D: ~A" (+ num 1) (elt *quote-list* num)))
           (caret-chat "There's no quotes - quick say something funny!")))
        ((= 0 (or (search "add " args) -1))
         (caret-chat "Quote ~D added by ~A"
                     (+ 1 (vector-push-extend (subseq args 4) *quote-list*))
                     (slot-value pl-entry 'name)))
        ((= 0 (or (search "remove " args) -1))
         (when (find-if (lambda (x) (string= x (slot-value pl-entry 'name)))
                        *quote-mods*)
           (let ((num (or (parse-integer (subseq args 7) :junk-allowed t)
                          -1)))
             (if (<= 1 num (length *quote-list*))
               (progn
                 (setf *quote-list* (delete (elt *quote-list* (- num 1)) *quote-list*))
                 (caret-chat "Quote ~D removed" num))
               (caret-chat "~A: no such quote" (subseq args 7))))))
        ((string-equal "count" args)
         (caret-chat "There are currently ~D quotes" (length *quote-list*)))
        ((parse-integer args :junk-allowed t)
         (let ((num (parse-integer args :junk-allowed t)))
           (when (<= 1 num (length *quote-list*))
             (caret-chat "~D: ~A" num (elt *quote-list* (- num 1))))))))

(defun caret-persist-quote (stream)
  (print '(setf *quote-list* (make-array 1 :fill-pointer 0 :adjustable t)) stream)
  (map nil (lambda (q) (print `(vector-push-extend ,q *quote-list*) stream)) *quote-list*))
