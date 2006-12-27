; grammar:
; expr = whitespace expr | expr whitespace | expr '+' expr | expr '-' expr | dice
;
; LL form:
; expr = whitespace dice whitespace rest whitespace
; rest = rest | '+' expr | '-' expr | <>
;
; constant expressions:
; whitespace = { ' ' | TAB }
; dice = number | "d" number | number "d" number
; number = { '0' } [1-9] { [0-9] }

(defun roll (dice sides)
  (do ((i 0 (incf i)) (total 0)) ((>= i dice) total)
    (incf total (+ (random sides) 1))))

(defun dice-number (stream)
  (let ((s (with-output-to-string (string)
             (do ((char (peek-char nil stream nil #\Nul) (peek-char nil stream nil #\Nul)))
               ((not (digit-char-p char)))
               (write-char (read-char stream) string)))))
    (if (< 0 (length s))
      (values (parse-integer s))
      (error 'parse-error))))

(defun dice-dice (stream)
  (let ((c (peek-char nil stream nil #\Nul)))
    (case c
      (#\Nul (error 'parse-error))
      ((#\d #\D)
       (read-char stream)
       `(roll 1 ,(dice-number stream)))
      (otherwise
        (let ((die-count (dice-number stream))
              (c2 (peek-char nil stream nil #\Nul)))
          (case c2
            ((#\d #\D) (read-char stream) `(roll ,die-count ,(dice-number stream)))
            (otherwise die-count)))))))

(defun dice-whitespace (stream)
  (loop for char = (peek-char nil stream nil #\Nul)
        while (or (eq char #\Space) (eq char #\Tab))
        do (read-char stream)))

(defun dice-rest (stream die1)
  (let ((c (peek-char nil stream nil #\Nul)))
    (case c
      (#\Nul die1)
      ((#\Space #\Tab) (dice-whitespace stream) (dice-rest stream die1))
      (#\+ (read-char stream) `(+ ,die1 ,(dice-expr stream)))
      (#\- (read-char stream) `(- ,die1 ,(dice-expr stream)))
      (otherwise (error 'parse-error)))))

(defun dice-expr (stream)
  (dice-whitespace stream)
  (let* ((die1 (dice-dice stream))
         (dummy (dice-whitespace stream))
         (result (dice-rest stream die1)))
    (declare (ignore dummy))
    (dice-whitespace stream)
    result))
