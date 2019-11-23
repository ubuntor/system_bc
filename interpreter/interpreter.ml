open Core
open Ir

let rec subst v x expr =
  match expr with
  | Expr.Var x' -> if String.equal x x' then v else expr
  | Fun (ty, x', e) ->
      if String.equal x x' then expr else Fun (ty, x', subst v x e)
  | Z -> expr
  | S0 e -> S0 (subst v x e)
  | S1 e -> S1 (subst v x e)
  | App (e1, e2) -> App (subst v x e1, subst v x e2)
  | Case (e, (e0, (x1, e1), (x2, e2))) ->
      let e = subst v x e in
      let e0 = subst v x e0 in
      let e1 = if String.equal x x1 then e1 else subst v x e1 in
      let e2 = if String.equal x x2 then e2 else subst v x e2 in
      Case (e, (e0, (x1, e1), (x2, e2)))
  | Rec (e, (e0, (x1, y1, e1), (x2, y2, e2))) ->
      let e = subst v x e in
      let e0 = subst v x e0 in
      let e1 =
        if String.equal x x1 || String.equal x y1 then e1 else subst v x e1
      in
      let e2 =
        if String.equal x x2 || String.equal x y2 then e2 else subst v x e2
      in
      Rec (e, (e0, (x1, y1, e1), (x2, y2, e2)))

let rec evaluate expr =
  match expr with
  | Expr.Var _ -> failwith "impossible: free variable"
  | Fun _ | Z -> expr
  | S0 e ->
      let v = evaluate e in
      S0 v
  | S1 e ->
      let v = evaluate e in
      S1 v
  | App (e1, e2) -> (
      let v1 = evaluate e1 in
      let v2 = evaluate e2 in
      match v1 with
      | Fun (_, x, e) -> evaluate (subst v2 x e)
      | _ -> failwith "impossible: application with non-fun" )
  | Case (e, (e0, (x1, e1), (x2, e2))) -> (
      let v = evaluate e in
      match v with
      | Expr.Z -> evaluate e0
      | S0 v' -> evaluate (subst v' x1 e1)
      | S1 v' -> evaluate (subst v' x2 e2)
      | _ -> failwith "impossible: case with non-nat" )
  | Rec (e, (e0, (x1, y1, e1), (x2, y2, e2))) -> (
      let v = evaluate e in
      match v with
      | Expr.Z -> evaluate e0
      | S0 v' ->
          let vr = evaluate (Rec (v', (e0, (x1, y1, e1), (x2, y2, e2)))) in
          evaluate (subst v' x1 (subst vr y1 e1))
      | S1 v' ->
          let vr = evaluate (Rec (v', (e0, (x1, y1, e1), (x2, y2, e2)))) in
          evaluate (subst v' x2 (subst vr y2 e2))
      | _ -> failwith "impossible: case with non-nat" )
