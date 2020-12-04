;;;; tetris.asd

(asdf:defsystem #:tetris
  :serial t
  :description "tetris: a simple game"
  :version "0.0.1"
  :author "Ismael Bejarano <ismaelbej@gmail.com>"
  :licence "MIT"
  :depends-on (#:lispbuilder-sdl)
  :components ((:file "package")
               (:file "tetris")))
