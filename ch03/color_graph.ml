open Asm

let edge_to_graph edges =
  let graph = Hashtbl.create 100 in
  let add_one_dir v1 v2 =
    let curr = match Hashtbl.find_opt graph v1 with
      | Some xs -> xs
      | None -> []
    in
    Hashtbl.replace graph v1 (v2 :: curr)
  in
  let add_edge_to_graph (v1, v2) =
    add_one_dir v1 v2; add_one_dir v2 v1
  in
  List.iter add_edge_to_graph edges; graph
