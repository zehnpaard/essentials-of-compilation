open Ast

let rec make_let alist e = match alist with
  | [] -> e
  | (t, e') :: alist' -> make_let alist' (Let (t, e', e))

let rec decomplex_exp ti e = match e with
  | Read | Int _ | Var _ -> e
  | Neg x ->
      let (e, alist) = decomplex_arg ti x in
      make_let alist e
  | Add (x1, x2) ->
      let (e1, alist1) = decomplex_arg ti x1 in
      let (e2, alist2) = decomplex_arg ti x2 in
      make_let (alist1 @ alist2) (Add (e1, e2))
  | Let (v, e', b) -> Let (v, decomplex_exp ti e', decomplex_exp ti b)
and decomplex_arg ti e = match e with
  | Int _ | Var _ -> (e, [])
  | _ -> 
      let t = "tmp." ^ string_of_int (ti := !ti + 1; !ti) in
      (Var t, [(t, decomplex_exp ti e)])

let f =
  let ti = ref 0 in
  function Program (info, e) -> Program (info, decomplex_exp ti e)
