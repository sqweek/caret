(in-package #:cl-user)
(declaim (optimize (debug 3) (speed 0) (safety 3)))

(defpackage :cmc (:use #:cl #:asdf))

(in-package :cmc)

(defsystem cmcbot
           :name "cmcbot"
           :author "sqweek"
           :depends-on (:trivial-http :cl-ppcre :local-time :sb-bsd-sockets :sb-posix)
           :components ((:file "settings")
                        (:file "http" :depends-on ("settings"))
                        (:file "mm" :depends-on ("settings"))
                        (:file "dice")
                        (:file "log" :depends-on ("mm"))
                        (:file "caret" :depends-on ("settings" "http" "mm"))
                        (:file "fun" :depends-on ("caret" "dice"))
                        (:file "persist" :depends-on ("caret" "mm"))
                        (:file "time" :depends-on ("caret" "mm" "persist"))
                        (:file "quote" :depends-on ("caret" "persist"))
                        ))
