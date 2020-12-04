# tetris-opengl
## Description

A simple tetris clone in lisp using opengl

## Requirements

* SBCL: `sudo apt-get install sbcl`.
* quicklisp: See [project page](https://www.quicklisp.org/beta/).
* lispbuilder-sdl: Execute `sudo apt-get install libsdl1.2-dev` in Ubuntu 20.04.
  For more detail go to (lispbuilder repo)[https://github.com/lispbuilder/lispbuilder].
  Install examples to check if it is working, from SBCL `(ql:quickload :lispbuilder-sdl-examples)`.

## Usage

* Copy all project files to ~/quicklisp/local-projects/tetris-opengl
* Load from SBCL execute (ql:quickload :tetris-opengl)
* Play (tetris-opengl:play)


### Keyboard

* Up: Rotate
* Down: Go down one line
* Left: Go left
* Right: Go right
* ESC to quit

Note: Sorry, I didn't use AWSD during development.

## License

MIT
