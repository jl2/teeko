;;;; teeko.asd
;;;;
;;;; Copyright (c) 2019 Jeremiah LaRocco <jeremiah_larocco@fastmailcom>

(asdf:defsystem #:teeko
  :description "Teeko game"
  :author "Jeremiah LaRocco <jeremiah_larocco@fastmail.com>"
  :license "ISC (BSD-like)"
  :depends-on (#:alexandria
               #:qtools
               #:qtgui
               #:qtcore
               #:mixalot
               #:trivial-main-thread)
  :serial t
  :components ((:file "package")
               (:file "teeko")
               (:file "gui")))

