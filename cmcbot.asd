(in-package #:cl-user)
(declaim (optimize (debug 3) (speed 0) (safety 3)))

(defpackage :cmc (:use #:cl #:asdf))

(in-package :cmc)

(defsystem cmcbot
           :name "cmcbot"
           :author "sqweek"
           :depends-on (:trivial-http :trivial-sockets :cl-ppcre :local-time :sb-posix)
           :components ((:file "settings")
                        (:file "http" :depends-on ("settings"))
                        (:file "mm" :depends-on ("settings"))
                        (:file "dice")
                        (:file "caret" :depends-on ("settings" "http" "mm"))
                        (:file "fun" :depends-on ("caret" "dice"))
                        (:file "log" :depends-on ("caret" "mm"))
                        (:file "persist" :depends-on ("caret" "mm"))
                        (:file "time" :depends-on ("caret" "mm" "persist"))
                        (:file "quote" :depends-on ("caret" "persist"))
                        ))
