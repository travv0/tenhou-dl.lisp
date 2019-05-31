;;;; tenhou-dl.asd

(asdf:defsystem #:tenhou-dl
  :description "Describe tenhou-dl here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:drakma
               :cl-html-parse
               :trivial-download
               :cl-fad
               :lparallel
               :cl-cpus)
  :components ((:file "package")
               (:file "tenhou-dl")))
