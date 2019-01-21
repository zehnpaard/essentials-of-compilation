let f p =
  Uniquify.f p
  |> Decomplex.f
  |> Explicate.f
  |> Select_instructions.f
  |> Uncover_live.f
  |> Build_interference.f
  |> Color_graph.f
  |> Allocate_reg.f
  |> List_vars.f
  |> Assign_home.f
  |> Patch_instructions.f
