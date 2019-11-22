open Core
open Ir

let empty_context = String.Map.empty

(* Maintain invariant that delta and gamma are disjoint *)
let insert_delta (delta, gamma) x ty =
  let delta = String.Map.set delta ~key:x ~data:ty in
  let gamma = String.Map.remove gamma x in
  (delta, gamma)

let insert_gamma (delta, gamma) x ty =
  let delta = String.Map.remove delta x in
  let gamma = String.Map.set gamma ~key:x ~data:ty in
  (delta, gamma)

let rec infer expr ~context : Ty.t Or_error.t =
  let open Or_error.Let_syntax in
  let assert_ty e ~ty ~error ~context =
    let%bind t = infer e ~context in
    if Ty.equal t ty then return ()
    else Or_error.error_s [%message error (ty : Ty.t)]
  in
  match expr with
  | Expr.Var x -> (
      let delta, gamma = context in
      match Map.find delta x with
      | Some ty -> Ok ty
      | None -> (
          match Map.find gamma x with
          | Some ty -> Ok ty
          | None ->
              Or_error.error_s [%message "variable not in context" (x : string)]
          ) )
  | Fun _ -> Or_error.error_s [%message "not implemented"] (* TODO *)
  | Z -> Ok Nat
  | S0 e | S1 e ->
      let%bind _ = assert_ty e ~ty:Nat ~error:"successor of non-nat" ~context in
      Ok Ty.Nat
  | App _ -> Or_error.error_s [%message "not implemented"] (* TODO *)
  | Case (e, (e0, (x1, e1), (x2, e2))) ->
      let%bind _ = assert_ty e ~ty:Nat ~error:"case: e not nat" ~context in
      let%bind _ = assert_ty e0 ~ty:Nat ~error:"case: e0 not nat" ~context in
      let%bind _ =
        assert_ty e1 ~ty:Nat ~error:"case: e1 not nat"
          ~context:(insert_gamma context x1 Nat)
      in
      let%bind _ =
        assert_ty e2 ~ty:Nat ~error:"case: e2 not nat"
          ~context:(insert_gamma context x2 Nat)
      in
      Ok Ty.Nat
  | Rec (e, (e0, (x1, y1, e1), (x2, y2, e2))) ->
      let delta, _ = context in
      let%bind _ =
        assert_ty e ~ty:Nat ~error:"rec: e not nat"
          ~context:(delta, empty_context)
      in
      let%bind _ = assert_ty e0 ~ty:Nat ~error:"rec: e0 not nat" ~context in
      let%bind _ =
        assert_ty e1 ~ty:Nat ~error:"rec: e1 not nat"
          ~context:(insert_gamma (insert_delta context x1 Nat) y1 Nat)
      in
      let%bind _ =
        assert_ty e2 ~ty:Nat ~error:"rec: e2 not nat"
          ~context:(insert_gamma (insert_delta context x2 Nat) y2 Nat)
      in
      Ok Ty.Nat

let typecheck expr = infer expr ~context:(empty_context, empty_context)
