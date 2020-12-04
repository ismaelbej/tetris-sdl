# tetris-sdl
## Description

A simple tetris clone in lisp using opengl

## Requirements

* SBCL
* quicklisp
* lispbuilder-sdl

### lispbuilder-sdl requires libsdl1.2

In ubuntu 20.04

    $ sudo apt-get install libsdl1.2-dev

## Usage

* Copy all project files to ~/quicklisp/local-projects/tetris-sdl
* Load from SBCL execute (ql:quickload :tetris-sdl)
* Play (tetris-sdl:play)


### Keyboard

* Arrows (sorry, AWSD was used during development)
* ESC to quit

## License

MIT

