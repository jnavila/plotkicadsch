open! StdLabels
open Lwt.Infix
open Kicadsch.Sigs

include DiffTool

module L = Kicadsch.MakeSchPainter(ListPainter.L)

module LP = struct
  include L

  type painterContext = ListPainter.listcanevas
end

let internal_diff (d : string) (c : SvgPainter.diff_colors option) (z: string option) =
  ( module struct
    let doc = "internal diff and show with " ^ d

    type pctx = ListPainter.listcanevas

    module S = LP

    type diff_style = Theirs | Ours | Idem

    let plot_elt style out_ctx (arg : ListPainter.t) =
      let open ListPainter in
      let module O = SvgPainter in
      let kolor =
        match style with
        | Theirs ->
          `Old
        | Ours ->
          `New
        | Idem ->
          `ForeGround
      in
      match arg with
      | Text (_, text, o, c, s, j, style) ->
        O.paint_text ~kolor text o c s j style out_ctx
      | Line (_, s, from_, to_) ->
        O.paint_line ~kolor ~width:s from_ to_ out_ctx
      | Rect (_, _, c1, c2) ->
        O.paint_rect ~kolor c1 c2 out_ctx
      | Circle (_, _, center, radius) ->
        O.paint_circle ~kolor center radius out_ctx
      | Arc (_, _, center, start_, end_, radius) ->
        O.paint_arc ~kolor center start_ end_ radius out_ctx
      | Image (corner, scale, data) ->
        O.paint_image corner scale data out_ctx
      | Format (Coord (x, y)) ->
        O.set_canevas_size x y out_ctx
      | Zone (c1, c2) ->
        O.paint_zone c1 c2 out_ctx

    let text_bbox text o c s j =
      (* TODO: vertical text does not work *)
      let len = String.length text in
      let Size sz = s in
      let Coord (x,y) = c in
      let shift =
        match j with
        | J_right | J_bottom -> -sz*len/2
        | J_center -> - sz*len/4
        | J_left | J_top -> 0
      in
      match o with
      | Orient_H ->
        BoundingBox.create_from_rect (Coord (x+shift,y)) (Coord (sz*len/2,sz/2))
      | Orient_V ->
        BoundingBox.create_from_rect (Coord (x, y-sz*len/2+shift)) (Coord (sz/2, sz*len/2))

    let elt_rect elt =
      let open ListPainter in
      let module BB = BoundingBox in
      match elt with
      | Text (_, text, o, c, s, j, _) ->
        text_bbox text o c s j
      | Line (_, _, f, t) ->
        BB.create_from_limits f t
      | Rect (_,  _, c1, c2)
      | Zone (c1, c2) ->
        BB.create_from_rect c1 c2
      | Circle (_, _, center, radius) ->
        let Coord(x,y) = center in
        BB.create_from_limits (Coord(x-radius, y-radius)) (Coord(x+radius,y+radius))
      | Arc (_ , _, center, _, _, radius) ->
        (* TODO: take into count partial angle *)
        let Coord(x,y) = center in
        BB.create_from_limits (Coord(x-radius, y-radius)) (Coord(x+radius,y+radius))
      | Image (corner, _, data) ->
        let w, h = SvgPainter.get_png_dims data in
        BB.create_from_rect corner (Coord(w, h))
      | Format _ -> BB.create ()

    let  dispatch_rect (res, acc) elt =
      if (BoundingBox.overlap_ratio res elt) > 0.9 then
        BoundingBox.add_rect res elt , acc
      else
          res, elt::acc

    let rec aggregate rect rect_list =
      let result, remaining = List.fold_left  ~f:dispatch_rect ~init:(rect, []) rect_list in
      if Int.equal (List.length remaining) (List.length rect_list) then
        result, remaining
      else
        aggregate result remaining

    let merge_rects rects:BoundingBox.t list =
      let rec aggregate_list out_list = function
        | rect::l ->
          let res, remaining = aggregate rect l in
          let res2, remaining2 = aggregate res out_list in
          aggregate_list (res2::remaining2) remaining
        | [] -> out_list , [] in
      fst (aggregate_list [] rects)

    let draw_bb ctx r =
      let c1, c2 = BoundingBox.as_rect r  in
      SvgPainter.paint_zone c1 c2 ctx

    let refine_segments (Coord (x1, y1), _) (Coord (x1', y1'), Coord (x2', y2')) =
      if (Int.compare x1 x1' == 0)
      then
        ((Int.compare y1 y1') * (Int.compare x1 x2'))
      else
        Int.compare y1 y2'

    let compare s1 s2 : int =
      let s1_r = elt_rect s1
      and s2_r = elt_rect s2 in
      let bb_comp = BoundingBox.compare s1_r s2_r in
      if bb_comp == 0 then
        match s1, s2 with
        | Text (_, t1, _, _, _, _ , _), Text (_, t2, _, _, _, _, _) -> String.compare t1 t2
        | Rect _, Rect _ -> 0
        | Line (_, _ , c1, c2), Line(_, _, c1', c2') -> refine_segments (c1, c2) (c1', c2')
        | Circle _, Circle _ -> 0
        | Arc _, Arc _ -> 0
        | Image _, Image _ -> 0
        | Zone _, Zone _ -> 0
        | Format _, Format _ -> 0
        | _, _ -> 1
      else bb_comp

    let draw_difftotal ~prev ~next out_canevas =
    let rec rec_draw_difftotal ~prev ~next (idem, theirs, ours, outc) diff_list =
      let r s = BoundingBox.reformat ~min_size:20 ~extend:50 (elt_rect s) in
      match prev, next with
      | p::pl, n::nl ->
        let comp = compare p n in
        if comp == 0 then
          rec_draw_difftotal ~prev:pl ~next:nl ((plot_elt Idem idem p),theirs, ours, outc) diff_list
        else if comp < 0 then
          rec_draw_difftotal ~prev:pl ~next (idem, (plot_elt Theirs theirs p), ours, outc) ((r p)::diff_list)
        else
          rec_draw_difftotal ~prev ~next:nl (idem, theirs, (plot_elt Ours ours n), outc) (r n::diff_list)
      | p::pl, [] ->
        rec_draw_difftotal ~prev:pl ~next (idem, (plot_elt Theirs theirs p), ours, outc) (r p::diff_list)
      | [], n::nl ->
        rec_draw_difftotal ~prev ~next:nl (idem, theirs, (plot_elt Ours ours n), outc) (r n::diff_list)
      |[],[] -> SvgPainter.(add_to theirs (add_to ours (add_to idem outc))), diff_list
    in
    let new_ctx = SvgPainter.new_from out_canevas in
    rec_draw_difftotal ~prev ~next (new_ctx, new_ctx, new_ctx, out_canevas) []

    let display_diff ~from_ctx ~to_ctx (filename:string list) ~keep =
      let prev = List.sort ~cmp:compare from_ctx in
      let next = List.sort ~cmp:compare to_ctx in
      match
        draw_difftotal ~prev ~next (SvgPainter.get_color_context c z)
      with
      | _, [] ->
        Lwt.return false
      | outctx, diff_list ->
        let merged_rects = merge_rects diff_list in
        let outctx = List.fold_left ~f:draw_bb ~init:outctx merged_rects in
        let svg_name = SysAbst.build_tmp_svg_name ~keep "diff_" filename in
        let open UnixLabels in
        let wait_for_1_s result =
          match result with
          | WSIGNALED n ->
            Printf.printf "signalled with signal %d\n" n ;
            Lwt.return svg_name
          | WSTOPPED n ->
            Printf.printf "stopped with %d\n" n ;
            Lwt.return svg_name
          | WEXITED err -> (
              match err with
              | 127 ->
                Printf.printf "Command not found: %s\n" d ;
                Lwt.return svg_name
              | 0 ->
                let t, u = Lwt.wait () in
                let erase_timeout =
                  Lwt_timeout.create 1 (fun () -> Lwt.wakeup u svg_name)
                in
                Lwt_timeout.start erase_timeout ;
                t
              | _ ->
                Printf.printf "Errored with code %d\n" err ;
                Lwt.return svg_name )
        in
        Lwt_io.with_file ~mode:Lwt_io.Output svg_name (fun o ->
            Lwt_io.write o @@ SvgPainter.write ~op:false outctx )
        >>= fun _ ->
        SysAbst.exec d [|svg_name|]
        >>= wait_for_1_s
        >>= SysAbst.finalize_tmp_file ~keep
        >|= fun _ -> true
  end
  : Differ )
