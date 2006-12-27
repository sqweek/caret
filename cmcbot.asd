(in-package #:cl-user)
(declaim (optimize (debug 3) (speed 0) (safety 3)))

(defpackage :cmc (:use #:cl #:asdf))

(in-package :cmc)

(defsystem cmcbot
           :name "cmcbot"
           :author "sqweek"
           :depends-on (:trivial-http :trivial-sockets :cl-ppcre :local-time :sb-posix)
           :components ((:file "settings")
                        (:file "http")
                        (:file "mm")
                        (:file "dice")
                        (:file "caret"))
           :serial t)
