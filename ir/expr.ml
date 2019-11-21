open! Core

type t =
  | Z
  | S0 of t
  | S1 of t
  | App of t * t
  | Case of t * (t * (string * t) * (string * t))
[@@deriving sexp]
