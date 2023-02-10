module Sigs = KicadSch_sigs
open Sigs
open KicadLib_sigs
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
    }

  type painterContext = P.t

  ;;

  let initial_context ?allow_missing_component:(allow_missing_component=false) rev =
    {lib=lib();canevas=EltPainter.create (P.get_context ()); rev; allow_missing_component}


  let orient_of_rot = function
    | 0 -> J_left
    | 90 -> J_top
    | 180 -> J_right
    | 270 -> J_bottom
    | s -> failwith ("unknown angle " ^ string_of_int s)

  let parse_schematics initctx content_tree =
    let sch_expr =
      field "kicad_sch"
        (fields
           ~default:initctx
           [
             ("version", int >>| fun _ args -> args)
           ; ("generator", string ~escaped:false  >>| fun _ args -> args)
           ; ("paper", paper_size_args >>| fun s args -> {args with canevas=EltPainter.draw_page_frame s args.canevas})
           ; ("uuid", uuid_args >>| fun _ args -> args)
           ; ("title_block", float >>| fun _s args -> args) (* TODO *)
           ; ("lib_symbols", lib_symbols_args >>| (fun s args -> {args with lib=List.fold_left ~init:args.lib ~f:(fun alib comp -> KicadLib_sigs.add_component comp alib) s}))
           ; ("junction", junction_args >>| (fun c args -> {args with canevas=EltPainter.draw_junction c args.canevas}))
           ; ("no_connect", no_connect_args >>| (fun c args -> {args with canevas=EltPainter.draw_no_connect c args.canevas}))
           ; ("wire", bus_wire_args >>| (fun l args -> {args with canevas=EltPainter.draw_wire l false args.canevas}))
           ; ("bus", bus_wire_args >>| (fun l args -> {args with canevas=EltPainter.draw_bus l false args.canevas}))
           ; ("text", text_gen_args >>| (fun (c, text, size, rot) args ->
               let orient = orient_of_rot rot in
               let l={c; size; orient;labeltype=TextLabel TextNote} in
               {args with canevas=EltPainter.draw_text_line text l args.canevas}))
           ; ("bus_entry", bus_entry_args >>|
              (fun ((Coord (xs, ys) as c), (xe, ye)) args ->
                 let end_point = Coord (xs+xe, ys+ye) in
                 {args with canevas=EltPainter.draw_wire [c; end_point] true args.canevas}))
           ; ("label", label_args >>| (fun (c, rot, text, size, _orient) args ->
               let orient = orient_of_rot rot in
               let label = {c; size; orient;labeltype=TextLabel WireLabel} in
               {args with canevas=EltPainter.draw_label text label args.canevas}))
           ; ("hierarchical_label", hierarchical_label_args >>| (fun (c, rot, text, size, shape, _orient) args ->
               let orient = orient_of_rot rot in
               let label = {c; size; orient; labeltype=PortLabel (Hlabel, shape)} in
               {args with canevas=EltPainter.draw_label text label args.canevas}))
           ; ("global_label", hierarchical_label_args >>| (fun (c, rot, text, size, shape, _orient) args ->
               let orient = orient_of_rot rot in
               let label = {c; size; orient; labeltype=PortLabel (Glabel, shape)} in
               {args with canevas=EltPainter.draw_label text label args.canevas}))
           ; ("polyline", polyline_args >>| (fun (_s, l) args -> {args with canevas=EltPainter.draw_line l args.canevas}))
           ; ("symbol", sch_symbol_args >>| (fun sym args ->
               let transfo =
                 let angle_rad = float_of_int sym.rot /. 180. *. Float.pi in
                 let cos_val = int_of_float (cos angle_rad) in
                 let sin_val = int_of_float (sin angle_rad) in
                 ((cos_val, sin_val), (-sin_val, cos_val)) in
               let cpaint = CPainter.plot_comp args.lib sym.lib_id sym.unit_nr sym.pos transfo args.allow_missing_component in
               let new_canevas, is_multi = EltPainter.modify_canevas cpaint args.canevas in
               let canevas = List.fold_left ~f:(fun canevas prop -> EltPainter.draw_field sym.pos transfo is_multi [] canevas (field_build prop)) ~init:new_canevas sym.properties in
               {args with canevas}))

           ]
        )
    in
    match Decode.run sch_expr content_tree with
    | Some res -> res
    | None -> (print_endline "decode failed!";initctx)

  let add_lib _content ctxt = ctxt

  let parse_sheet initctx content =
    let tree_opt = Parsexp.Single.parse_string content in
    match tree_opt with
    | Ok tree -> parse_schematics initctx tree
    | Error _error -> failwith ("content is not correct sexp: ") (* TODO *)

  let output_context (ctx: schContext): painterContext = EltPainter.get_context ctx.canevas

end
