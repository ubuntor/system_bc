# SdL Compiler

## Build Instructions

Make sure `opam` is installed, then install `dune`, `core`, `ppx_jane`,
`menhir` from `opam`.

Then run `make`. The executable will be in `_build/default/sdl_compiler.exe`.

## Structure
  - `system_bc.ml`: Toplevel commandline. Runs all phases of compilation.
  - `parse/`: Parser and lexer.
  - `check/`: Static typechecking.
  - `interpreter/`: Interpreter
