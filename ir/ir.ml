open! Core

module Ty = struct
  type t = Nat | Arr of t * t | Boxarr of t * t [@@deriving sexp]
end

module Expr = struct
  type t =
    | Var of string
    | Fun of Ty.t * string * t
    | Z
    | S0 of t
    | S1 of t
    | App of t * t
    | Case of t * (t * (string * t) * (string * t))
    | Rec of t * (t * (string * string * t) * (string * string * t))
  [@@deriving sexp]
end
