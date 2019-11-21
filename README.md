# SdL Compiler

## Build Instructions

Make sure `opam` is installed, then install `dune`, `core`, `ppx_jane`,
`menhir` from `opam`.

Then run `make`. The executable will be in `_build/default/sdl_compiler.exe`.

You can also run `dune exec -- ./system_bc.exe FILENAME` to run the interpreter.

## Structure
  - `system_bc.ml`: Toplevel commandline. Runs all phases of compilation.
  - `ir/`: Intermediate representations.
  - `parse/`: Parser and lexer.
  - `check/`: Static typechecking.
  - `interpreter/`: Interpreter.
  - `examples/`: Example System BC files.

## Syntax
```
Type t ::=
    | nat
    | t1 -> t2               Arrows associate to the right.
    | [](t1 -> t2)           Boxed arrows require explicit parentheses.

Expr e ::=
    | x
    | fun (x : tau) e
    | (e1)(e2)               Function application requires explicit parentheses.
    | z
    | s0(e)
    | s1(e)
    | case e {
          z -> e0
        | s0(x) -> e1
        | s1(y) -> e2
      }
    | rec e {
          z -> e0
        | s0(x1) with y1 -> e1
        | s1(x2) with y2 -> e2
      }
```
