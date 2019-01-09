open Ast

let tmp_i = ref 0

let rec make_let alist e = match alist with
  | [] -> e
  | (t, e') :: alist' -> make_let alist' (Let (t, e', e))

let rec decomplex_exp e = match e with
  | Read | Int _ | Var _ -> e
  | Neg x ->
      let (e, alist) = decomplex_arg x in
      make_let alist e
  | Add (x1, x2) ->
      let (e1, alist1) = decomplex_arg x1 in
      let (e2, alist2) = decomplex_arg x2 in
      make_let (alist1 @ alist2) (Add (e1, e2))
  | Let (v, e', b) -> Let (v, decomplex_exp e', decomplex_exp b)
      
and decomplex_arg e = match e with
  | Int _ | Var _ -> (e, [])
  | _ -> 
      let t = "tmp." ^ string_to_int (tmp_i := !tmp_i + 1; !tmp_i)
      (Var t, [(t, decomplex_exp e)])
