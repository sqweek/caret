(defun caret-cmd-boggle (pl-entry msg)
  (declare (ignore pl-entry msg))
  (caret-chat "Ha, Gorilla, right there!"))

(defun caret-cmd-dice (pl-entry msg)
  "Roll some dice. eg, ^dice d20+4-2d6"
  (with-slots (name) pl-entry
    (handler-case
      (with-timeout 1
        (caret-chat "~A: ~A = ~D" name msg
                    (eval (with-input-from-string (s msg) (dice-expr s)))))
      (parse-error () (caret-chat "~A: ~A = Syntax Error" name msg))
      (timeout () (caret-chat "~A: ~A = No." name msg)))))

(defun caret-cmd-eggplant (pl-entry msg)
  (declare (ignore pl-entry msg))
  (caret-chat "Meal of the moment: eggplant ~A" (random-word "foccacia" "quiche" "omelette" "bolognese" "pasta" "lasagne" "parmesan" "parmelia" "parmigiana" "lemonade" "soup" "sandwiches" "burgers" "sushi" "voulavants" "jaffles" "waffles")))
