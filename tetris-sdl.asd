;;;; tetris-sdl.asd

(asdf:defsystem #:tetris-sdl
  :description "a simple game tetris clone"
  :author "Ismael Bejarano <ismaelbej@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:lispbuilder-sdl)
  :components ((:file "package")
               (:file "tetris-sdl")))
