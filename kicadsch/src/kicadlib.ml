open KicadSch_sigs

module MakePainter (P : Painter) : CompPainter with type drawContext := P.t =
struct
  open KicadLib_sigs

  type t = library

  type drawContext = P.t



  let ( +$ ) (Coord (x1, y1)) (RelCoord (x2, y2)) = Coord (x1 + x2, y1 + y2)

  let ( *$ ) ((a, b), (c, d)) (RelCoord (x, y)) =
    RelCoord ((a * x) + (b * y), (c * x) + (d * y))

  let rotate (origin : coord) (rotation : transfo) (relpoint : relcoord) :
      coord =
    origin +$ (rotation *$ relpoint)

  let rec plot_poly rotfun thickness points ctx =
    match points with
    | [] | [_] ->
        ctx
    | c1 :: c2 :: tl ->
        let c1' = rotfun c1 in
        let c2' = rotfun c2 in
        plot_poly rotfun thickness (c2 :: tl) (P.paint_line c1' c2' ctx)

  let plot_pin rotfun {name; number; length; contact; orient} c ctx =
    let (RelCoord (x, y)) = contact in
    let (Size delta) = length in
    let sc =
      match orient with
      | P_R ->
          RelCoord (x + delta, y)
      | P_L ->
          RelCoord (x - delta, y)
      | P_U ->
          RelCoord (x, y + delta)
      | P_D ->
          RelCoord (x, y - delta)
    in
    let (Coord (nxsc, nysc) as new_sc) = rotfun sc in
    let (Coord (nx, ny) as new_contact) = rotfun contact in
    let new_J, new_orient =
      if nx > nxsc then (J_right, Orient_H)
      else if nx < nxsc then (J_left, Orient_H)
      else if ny > nysc then (J_top, Orient_V)
      else (J_bottom, Orient_V)
    in
    let name_text, name_size = name in
    let pin_text, pin_size = number in
    let pin_ctx = P.paint_line new_sc new_contact ctx in
    let pname_ctx =
      if c.draw_pname && String.compare "~" name_text <> 0 then
        P.paint_text name_text new_orient new_sc name_size new_J NoStyle
          pin_ctx
      else pin_ctx
    in
    if c.draw_pnum && String.compare "~" pin_text <> 0 then
      P.paint_text pin_text new_orient new_contact pin_size new_J NoStyle
        pname_ctx
    else pname_ctx

  let plot_elt rotfun comp part ctx {parts; prim} =
    if parts = 0 || parts = part then
      match prim with
      | Polygon (t, pts) ->
          plot_poly rotfun t pts ctx
      | Circle (_, {center; radius}) ->
          P.paint_circle (rotfun center) radius ctx
      | Field ->
          ctx
      | Pin p ->
          plot_pin rotfun p comp ctx
      | Text {c; text; s} ->
          P.paint_text text Orient_H (rotfun c) s J_left NoStyle ctx
      | Arc {radius; sp; ep; center; _} ->
        P.paint_arc (rotfun center) (rotfun sp) (rotfun ep) radius ctx
      (* TODO: paint Bezier *)
      | Bezier(_, _) -> ctx
    else ctx

  exception Component_Not_Found of string

  let plot_comp lib comp_name part rotation origin allow_missing (ctx : drawContext)
    =
    match get_comp lib comp_name with
    | Some thecomp ->
      let rot = rotate rotation origin in
      ( List.fold_left (plot_elt rot thecomp part) ctx thecomp.graph
    , thecomp.multi )
    | None -> if allow_missing then (ctx, false) else raise (Component_Not_Found comp_name)
end
