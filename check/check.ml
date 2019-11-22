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

let rec subtype ty1 ty2 =
  if Ty.equal ty1 ty2 then true
  else
    match (ty1, ty2) with
    | Arr (ty11, ty12), Arr (ty21, ty22) ->
        subtype ty21 ty11 && subtype ty12 ty22
    | Arr _, Boxarr (ty21, ty22) -> subtype ty1 (Arr (ty21, ty22))
    | _ -> false

let rec infer expr ~context : Ty.t Or_error.t =
  let open Or_error.Let_syntax in
  let assert_ty e ~ty ~error ~context =
    let%bind t = infer e ~context in
    if Ty.equal t ty then return ()
    else Or_error.error_s [%message error (ty : Ty.t)]
  in
  let delta, gamma = context in
  match expr with
  | Expr.Var x -> (
      match Map.find delta x with
      | Some ty -> Ok ty
      | None -> (
          match Map.find gamma x with
          | Some ty -> Ok ty
          | None ->
              Or_error.error_s [%message "var: not in context" (x : string)] ) )
  | Fun (ty1, x, e) -> (
      match infer e ~context:(insert_gamma context x ty1) with
      | Ok ty2 -> Ok (Arr (ty1, ty2))
      | Error _ -> (
          match infer e ~context:(insert_delta context x ty1) with
          | Ok ty2 -> Ok (Boxarr (ty1, ty2))
          | Error e ->
              Or_error.error_s [%message "fun: failed to type" (e : Error.t)] )
      )
  | Z -> Ok Nat
  | S0 e | S1 e ->
      let%bind _ = assert_ty e ~ty:Nat ~error:"successor: non-nat" ~context in
      Ok Ty.Nat
  | App (e1, e2) -> (
      let%bind arr_ty = infer e1 ~context in
      match arr_ty with
      | Arr (ty1, ty2) ->
          let%bind ty1' = infer e2 ~context in
          (* TODO check this *)
          if subtype ty1' ty1 then Ok ty2
          else
            Or_error.error_s
              [%message "app: type mismatch" (ty1 : Ty.t) (ty1' : Ty.t)]
      | Boxarr (ty1, ty2) ->
          let%bind ty1' = infer e2 ~context:(delta, empty_context) in
          (* TODO check this *)
          if subtype ty1' ty1 then Ok ty2
          else
            Or_error.error_s
              [%message "app: type mismatch" (ty1 : Ty.t) (ty1' : Ty.t)]
      | _ -> Or_error.error_s [%message "app: non-fun" (arr_ty : Ty.t)] )
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
