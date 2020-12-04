;;;; tetris-opengl.asd

(asdf:defsystem #:tetris-opengl
  :description "a simple game tetris clone"
  :author "Ismael Bejarano <ismaelbej@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:lispbuilder-sdl)
  :components ((:file "package")
               (:file "tetris-opengl")))
