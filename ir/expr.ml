open! Core

type t =
  | Z
  | S0 of t
  | S1 of t
  [@@deriving sexp]
