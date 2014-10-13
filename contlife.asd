;;;; contlife.asd

(asdf:defsystem #:contlife
  :serial t
  :description "Describe contlife here"
  :author "Jeremiah LaRocco <jeremiah.larocco@gmail.com>"
  :license "Specify license here"
  :depends-on (#:lispbuilder-sdl
               #:cl-opengl
               #:cl-glu)
  :components ((:file "package")
               (:file "contlife")))

