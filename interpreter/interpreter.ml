open Core
open Ir

let rec evaluate expr ~context =
  match expr with
  | Expr.Var x -> Map.find_exn context x
  | Fun _ | Z -> expr
  | S0 e ->
      let v = evaluate e ~context in
      S0 v
  | S1 e ->
      let v = evaluate e ~context in
      S1 v
  | App (e1, e2) -> (
      let v1 = evaluate e1 ~context in
      let v2 = evaluate e2 ~context in
      match v1 with
      | Fun (_, x, e) ->
          evaluate e ~context:(String.Map.set context ~key:x ~data:v2)
      | _ -> failwith "impossible: application with non-fun" )
  | Case (e, (e0, (x1, e1), (x2, e2))) -> (
      let v = evaluate e ~context in
      match v with
      | Expr.Z -> evaluate e0 ~context
      | S0 v' -> evaluate e1 ~context:(String.Map.set context ~key:x1 ~data:v')
      | S1 v' -> evaluate e2 ~context:(String.Map.set context ~key:x2 ~data:v')
      | _ -> failwith "impossible: case with non-nat" )
  | Rec (e, (e0, (x1, y1, e1), (x2, y2, e2))) -> (
      let v = evaluate e ~context in
      match v with
      | Expr.Z -> evaluate e0 ~context
      | S0 v' ->
          let vr =
            evaluate (Rec (v', (e0, (x1, y1, e1), (x2, y2, e2)))) ~context
          in
          evaluate e1
            ~context:
              (String.Map.set
                 (String.Map.set context ~key:x1 ~data:v')
                 ~key:y1 ~data:vr)
      | S1 v' ->
          let vr =
            evaluate (Rec (v', (e0, (x1, y1, e1), (x2, y2, e2)))) ~context
          in
          evaluate e2
            ~context:
              (String.Map.set
                 (String.Map.set context ~key:x2 ~data:v')
                 ~key:y2 ~data:vr)
      | _ -> failwith "impossible: case with non-nat" )

let interpret expr = evaluate expr ~context:String.Map.empty
