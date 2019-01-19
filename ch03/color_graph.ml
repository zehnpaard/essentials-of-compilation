open Asm

module S = Set.Make(struct let compare = compare type t = int end)

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

let find_min_absent xs =
  let ys = S.of_list xs in
  let rec f n = if S.mem n ys then f (n+1) else n in
  f 0

let process_node graph colors saturation node =
  let c = find_min_absent (Hashtbl.find saturation node) in
  let f node1 = Hashtbl.replace saturation node1 (c :: Hashtbl.find saturation node1) in
  begin
    Hashtbl.add colors node c;
    List.iter f (Hashtbl.find graph node)
  end


let color graph =
  let nodes = Hashtbl.to_seq_keys graph |> List.of_seq in
  let colors = Hashtbl.create (List.length nodes) in
  let saturation = Hashtbl.of_seq (List.to_seq (List.map (fun n -> (n, [])) nodes)) in
  colors, saturation

let f = function Program _ as p -> p
