This is a modified version of Conway's Game Of Life written in Common Lisp, using OpenGL and SDL.

This version of Conway's Game of Life replaces the on/off state of each cell with a floating point number between 0.0 and 1.0.  Each generation the sum of each cell's neighbors is calculated.  Cells whose sum is between 0.0 and 0.75 "live" and get a boost of 0.05, while other cells "die" and have 0.3 subtracted from their value.  The initial board is initialized with random numbers between 0 and 1.

The values of the life and death parameters, as well as animation speed can be controlled by passing a list of values into the start-life function.

Here is a sample usage and screenshot:

```commonlisp
* (ql:quickload 'contlife)
To load "contlife":
  Load 1 ASDF system:
    gllife
; Loading "contlife"

(CONTLIFE)
* (contlife:start-life :board-width 400 :board-height 400 :transition '(0.3 0.65 -0.2 0.05) :multiplier 3.0)
```

![Screenshot](http://www.laroccophoto.com/photos/i-FJXmQdV/0/XL/i-FJXmQdV-XL.png "Continuos Life Screenshot")
