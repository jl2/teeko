;;;; teeko.asd
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(asdf:defsystem #:teeko
  :description "Describe teeko here"
  :author "Jeremiah LaRocco <jeremiah.larocco@gmail.com>"
  :license "ISC (BSD-like)"
  :depends-on (#:qtools
               #:qtgui
               #:qtcore
               #:mixalot
               #:trivial-main-thread)
  :serial t
  :components ((:file "package")
               (:file "teeko")))

