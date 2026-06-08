module Sigs = KicadSch_sigs
open Sigs
open KicadLib_sigs
open KicadDefs
module Decode = Sexp_decode.Make(Base.Sexp)
open Decode
open! StdLabels
open SexpKicadSch

module MakeSchPainter (P : Painter) :
  SchPainter with type painterContext := P.t = struct
  module CPainter = Kicadlib.MakePainter (P)
  module EltPainter = SchElementPainter.MakePainter (P)

  type schContext =
    {
      lib: library
    ; canevas: EltPainter.t
    ; rev: revision
    ; allow_missing_component: bool
    ; base_coord: coord
    }

  type painterContext = P.t

  let file_extension = ".kicad_sch"
  ;;

  let initial_context ?allow_missing_component:(allow_missing_component=false) rev =
    {lib=lib();canevas=EltPainter.create (P.get_context ()); rev; allow_missing_component; base_coord=Coord (0, 0)}

  let orient_of_rot = fun rot j ->
    match rot, j with
    | 0, J_left -> J_left
    | 0, _ -> J_right
    | 90, _ -> J_top
    | 180, J_left -> J_right
    | 180, _ -> J_right
    | 270, _ -> J_bottom
    | s, _ -> failwith ("unknown angle " ^ string_of_int s)


  let parse_schematics initctx (content_tree, pos) =
    let sch_expr =
      field "kicad_sch"
        (fields
           ~default:initctx
           [
             ("version", int >>| fun _ args -> args)
           ; ("generator", string ~escaped:false  >>| fun _ args -> args)
           ; ("generator_version", string ~escaped:false >>| fun _ args -> args)
           ; ("paper", paper_size_args >>| fun base_coord args -> {args with canevas=EltPainter.draw_page_frame base_coord args.canevas; base_coord})
           ; ("title_block", title_block_v8_args >>| fun (title, date, rev, company, comments) args ->
              let Coord (x, y) = args.base_coord in
              let corner = Coord (x - 4000, y) in
              let canevas = args.canevas |>
                            EltPainter.draw_title_field corner "Title" title |>
                            EltPainter.draw_title_field corner "Date" date |>
                            EltPainter.draw_title_field corner "Rev" rev |>
                            EltPainter.draw_title_field corner "Comp" company in
              let canevas = List.fold_left ~init:canevas ~f:(fun cv (c, text) ->
                  EltPainter.draw_title_field corner ("Comment" ^ (string_of_int c)) text cv) comments in
              {args with canevas})
           ; ("uuid", uuid_args >>| fun _ args -> args)
           ; ("lib_symbols", lib_symbols_args >>| (fun s args ->
               {args with lib=List.fold_left ~init:args.lib ~f:(fun alib comp -> KicadLib_sigs.add_component comp alib) s}))
           ; ("junction", junction_args >>| (fun c args -> {args with canevas=EltPainter.draw_junction c args.canevas}))
           ; ("no_connect", no_connect_args >>| (fun c args -> {args with canevas=EltPainter.draw_no_connect c args.canevas}))
           ; ("wire", bus_wire_args >>| (fun l args -> {args with canevas=EltPainter.draw_wire l false args.canevas}))
           ; ("bus", bus_wire_args >>| (fun l args -> {args with canevas=EltPainter.draw_bus l false args.canevas}))
           ; ("rectangle", rectangle_args >>| (fun (s, e) args -> {args with canevas=EltPainter.draw_sheet_rect s e args.canevas}))
           ; ("text", text_gen_args >>| (fun (c, text, size, rot, j) args ->
               let orient = orient_of_rot rot j in
               let l={c; size; orient;labeltype=TextLabel TextNote} in
               {args with canevas=EltPainter.draw_text_line text l args.canevas}))
           ; ("bus_entry", bus_entry_args >>|
              (fun ((Coord (xs, ys) as c), (xe, ye)) args ->
                 let end_point = Coord (xs+xe, ys+ye) in
                 {args with canevas=EltPainter.draw_wire [c; end_point] true args.canevas}))
           ; ("label", label_args >>| (fun (c, rot, text, size, orient) args ->
               let orient = orient_of_rot rot orient in
               let label = {c; size; orient;labeltype=TextLabel WireLabel} in
               {args with canevas=EltPainter.draw_label text label args.canevas}))
           ; ("hierarchical_label", hierarchical_label_args >>| (fun (c, rot, text, size, shape, orient) args ->
               let orient = orient_of_rot rot orient in
               let label = {c; size; orient; labeltype=PortLabel (Hlabel, shape)} in
               {args with canevas=EltPainter.draw_label text label args.canevas}))
           ; ("global_label", hierarchical_label_args >>| (fun (c, rot, text, size, shape, orient) args ->
               let orient = orient_of_rot rot orient in
               let label = {c; size; orient; labeltype=PortLabel (Glabel, shape)} in
               {args with canevas=EltPainter.draw_label text label args.canevas}))
           ; ("directive_label", label_args >>| (fun (c, rot, text, size, orient) args ->
               let orient = orient_of_rot rot orient in
               let label = {c; size; orient; labeltype=TextLabel WireLabel} in
               {args with canevas=EltPainter.draw_label text label args.canevas}))
           ; ("netclass_flag", label_args >>| (fun (c, rot, text, size, orient) args ->
               let orient = orient_of_rot rot orient in
               let label = {c; size; orient; labeltype=TextLabel WireLabel} in
               {args with canevas=EltPainter.draw_label text label args.canevas}))
           ; ("polyline", polyline_args >>| (fun (_s, l) args -> {args with canevas=EltPainter.draw_line l args.canevas}))
           ; ("image", image_args >>| (fun b args ->
                 {args with canevas=EltPainter.draw_bitmap b args.canevas}))
           ; ("symbol", sch_symbol_args >>| (fun sym args ->
               let ((a11,a12),(a21,a22)) =
                 match sym.rot with
                 | 0 -> ((1, 0), (0, -1))
                 | 90 -> ((0, -1), (-1, 0))
                 | 180 -> ((-1, 0), (0, 1))
                 | 270 -> ((0, 1), (1, 0))
                 | s ->
                   let angle_rad = float_of_int s /. 180. *. Float.pi in
                   let cos_val = int_of_float (cos angle_rad) in
                   let sin_val = int_of_float (sin angle_rad) in
                   ((cos_val, sin_val), (-sin_val, cos_val)) in
               let invert_x = if sym.mirror_x then -1 else 1 in
               let invert_y = if sym.mirror_y then -1 else 1 in
               let transfo = ((invert_y * a11, invert_y * a12), (invert_x * a21, invert_x * a22)) in
                let lookup_key = Option.value sym.lib_name ~default:sym.lib_id in
                let cpaint = CPainter.plot_comp args.lib lookup_key sym.unit_nr sym.pos transfo args.allow_missing_component in
               let new_canevas, is_multi = EltPainter.modify_canevas cpaint args.canevas in
               let canevas = List.fold_left ~f:(fun canevas prop -> match (field_build prop) with None -> canevas | Some field -> EltPainter.draw_field prop.at transfo is_multi [] canevas field) ~init:new_canevas sym.properties in
               {args with canevas}))
           ; ("sheet", sheet_args_v8 >>| (fun (at, size, properties, hierachical_pins) args ->
               let cnv = EltPainter.draw_sheet_rect at size args.canevas in
               let cnv1 = List.fold_left ~init: cnv ~f:(fun cv {value; id; at; effects; _} -> EltPainter.draw_sheet_field value id (Size (Option.fold ~none:10 ~some:fontsize_of_effect effects)) at size cv) properties in
               let canevas = List.fold_left ~init:cnv1 ~f:(fun cv (name, port_type, justif, pos, s) -> EltPainter.draw_port name port_type justif pos s cv) hierachical_pins in
               {args with canevas}))
           (* V8-only top-level items *)
           ; ("arc", sch_arc_args >>| (fun arc_opt args ->
               match arc_opt with
               | None -> args
               | Some (center, start_pt, end_pt, radius) ->
                 {args with canevas=EltPainter.draw_arc center start_pt end_pt radius args.canevas}))
           ; ("circle", sch_circle_args >>| (fun (center, radius) args ->
               {args with canevas=EltPainter.draw_circle center radius args.canevas}))
           ; ("bezier", sch_bezier_args >>| (fun pts args ->
               match pts with
               | _::_::_ -> {args with canevas=EltPainter.draw_line pts args.canevas}
               | _ -> args))
           ; ("rule_area", sch_rule_area_args >>| (fun pts args ->
               match pts with
               | _::_::_ -> {args with canevas=EltPainter.draw_line pts args.canevas}
               | _ -> args))
           ; ("text_box", sch_text_box_args >>| (fun (corner, dim, text, effects) args ->
               let Coord (x, y) = corner and Coord (w, h) = dim in
               let cnv = EltPainter.draw_sheet_rect corner (Coord (x + w, y + h)) args.canevas in
               let sz = match effects with
                 | None -> Size 100
                 | Some e -> let Coord (_, sy) = e.font.size in Size sy in
               let orient = match effects with
                 | None -> J_left
                 | Some e -> justify_of_effect e in
               let label = {c=corner; size=sz; orient; labeltype=TextLabel TextNote} in
               {args with canevas=EltPainter.draw_text_line text label cnv}))
           (* items with no Painter interface at the moment *)
           ; ("ellipse",      skip_all >>| fun _ args -> args)
           ; ("ellipse_arc",  skip_all >>| fun _ args -> args)
           ; ("table",        skip_all >>| fun _ args -> args)
           ; ("net_chain",    skip_all >>| fun _ args -> args)
           ; ("group",        skip_all >>| fun _ args -> args)
           ; ("embedded_fonts",  skip_all >>| fun _ args -> args)
           ; ("embedded_files",  skip_all >>| fun _ args -> args)
           ; ("sheet_instances", repeat1_full_list sheet_path_instance_expr >>| (fun _ args -> args))
           ; ("symbol_instances", skip_all >>| fun _ args -> args)
           ; ("bus_alias", string ~escaped:true <*> skip >>| (fun _ args -> args))
           ]
        )
    in
    match Decode.run_with_result sch_expr content_tree with
    | Ok res -> res
    | Error sub ->
      (match Parsexp.Positions.find_sub_sexp_phys pos content_tree ~sub:sub with
       | Some err_range -> failwith (Format.sprintf "%d:%d: Decode failed for %s@." err_range.start_pos.line err_range.start_pos.col (Sexplib0.Sexp.to_string sub))
       | None -> failwith "decode failed!")

  let add_lib _content ctxt = ctxt

  let parse_sheet initctx content =
    let tree_opt = Parsexp.Single_and_positions.parse_string content in
    match tree_opt with
    | Ok tree -> parse_schematics initctx tree
    | Error _error -> failwith ("content is not correct sexp: ")

  let output_context (ctx: schContext): painterContext = EltPainter.get_context ctx.canevas

end
