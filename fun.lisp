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
