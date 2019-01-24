open Asm

module S = Set.Make(struct let compare = compare type t = int end)

let edges_to_graph edges =
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

let find_min_absent xs =
  let ys = S.of_list xs in
  let rec f n = if S.mem n ys then f (n+1) else n in
  f 0

let rec max_ saturation node v = function
  | [] -> node
  | node' :: nodes ->
    let v' = List.length (Hashtbl.find saturation node') in
    if v' > v
    then max_ saturation node' v' nodes
    else max_ saturation node v nodes

let color graph =
  let nodes = Hashtbl.to_seq_keys graph |> List.of_seq in
  let colors = Hashtbl.create (List.length nodes) in
  let saturation = Hashtbl.of_seq (List.to_seq (List.map (fun n -> (n, [])) nodes)) in
  let choose_node nodes =
    let n = List.hd nodes in
    let v = List.length (Hashtbl.find saturation n) in
    max_ saturation n v (List.tl nodes)
  in
  let process_node node =
    let c = find_min_absent (Hashtbl.find saturation node) in
    let f node1 = Hashtbl.replace saturation node1 (c :: Hashtbl.find saturation node1) in
    begin
      Hashtbl.add colors node c;
      List.iter f (Hashtbl.find graph node)
    end
  in
  let rec process = function
    | [] -> colors
    | nodes ->
      let node = choose_node nodes in
      let nodes' = List.filter ((<>) node) nodes in
      (process_node node; process nodes')
  in
  process nodes

let is_var_edge = function
  | (Var _, Var _) -> true
  | _ -> false

let modify_block (Block (info, instrs)) = match info.interference with
  | Some interference ->
    let edges = List.filter is_var_edge interference in
    let colors = color (edges_to_graph edges) in
    let info' = {info with colors=Some colors} in
    Block (info', instrs)
  | None -> failwith "Attempting graph coloring with no interference information"

let modify_nb (s, block) = (s, modify_block block)

let f (Program (info, nbs)) =
  Program (info, List.map modify_nb nbs)
