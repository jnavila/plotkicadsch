open KicadDefs
module Sigs = KicadSch_sigs
open Sigs
open KicadLib_sigs
module Decode = Sexp_decode.Make(Base.Sexp)
open Decode
open! StdLabels


let mm_size x = int_of_float (x *. 100.)
let wx_size x = int_of_float (x *. 100.)

let coords = float <*> float >>| (fun (x, y) -> Coord (wx_size x, wx_size y))


let dist_expr s =
  field s  float >>| (fun s -> wx_size s)
let length_expr =
  dist_expr "length"

type kolor = {red: int; green: int; blue: int; alpha: float}
type width = Width of int

let kolor_args =
  let+ colors = tuple4 int  int  int  float in
  let red, green, blue, alpha = colors in
  {red; green; blue; alpha}

let kolor_expr = field "color" kolor_args


let uuid_args = (string ~escaped:false) >>= (fun s -> match Uuidm.of_string s with Some u -> return u | None -> failwith "Not a uuid")
let gen_uuid_expr s =
  field s uuid_args

let uuid_expr = gen_uuid_expr "uuid"
let page_UUID_expr = gen_uuid_expr "uuid"
let page_expr = field "page" (string ~escaped:false)
;;

let paper_size_args =
  let* s = string ~escaped:false in
  let* size = maybe (coords) in
  let+ p = maybe (tag "portrait") in

  let Coord(x, y) as c = match s with
    | "A5" -> (Coord ((mm_size 210.), (mm_size 148.)))
    | "A4" -> (Coord ((mm_size 297.), (mm_size 210.)))
    | "A3" -> (Coord((mm_size  420.  ), (mm_size 297.  )))
    | "A2" -> (Coord((mm_size  594.  ), (mm_size 420.  )))
    | "A1" -> (Coord((mm_size  841.  ), (mm_size 594.  )))
    | "A0" -> (Coord((mm_size  1189. ), (mm_size 841.  )))
    | "A"  -> (Coord((wx_size  11000.), (wx_size  8500.)))
    | "B"  -> (Coord((wx_size  17000.), (wx_size 11000.)))
    | "C"  -> (Coord((wx_size  22000.), (wx_size 17000.)))
    | "D"  -> (Coord((wx_size  34000.), (wx_size 22000.)))
    | "E"  -> (Coord((wx_size  44000.), (wx_size 34000.)))
(*  | "GERBER" ->  coords *)
    | "User" ->  (match size with  Some coords -> coords | None -> failwith "User paper size requires a size")

    | "USLetter" -> (Coord((wx_size  11000.), (wx_size 8500.)))
    | "USLegal"  -> (Coord((wx_size  14000.), (wx_size 8500.)))
    | "USLedger" -> (Coord((wx_size  17000.), (wx_size 11000.)))
    | s -> failwith ("unknown paper size " ^ s) in
  match p with Some _ -> (Coord(y, x)) | None -> c

;;
let paper_expr =
  field "paper" paper_size_args
;;
let xy_expr =
  field "xy" coords

let pts_expr =
  field "pts" (repeat_full_list xy_expr)

let make_rel (Coord(x,y)) = RelCoord(x,y)
let make_rel_pts pts =
  List.map ~f:make_rel pts
;;

let string_expr s =
  field s (string ~escaped:false)

let int_expr s =
  field s int

let float_expr s =
  field s float

  (* (s yes) (s no) or (s) *)
let yesno_expr s =
  field s (maybe atom) >>= (function | Some "yes" -> return true | Some "no" -> return false | _ -> return true)

;;

let pin_at_coord_args =
  let+ at_coords = tuple3 float float (maybe int) in
  let x, y, angle = at_coords in
  Coord (wx_size x, wx_size y), Option.value ~default:0 angle

let pin_at_coord_expr = field "at" pin_at_coord_args

;;

type fill_type =
  | No_fill
  | Background_fill
  | Outline_fill

let fill_type_atom = atom >>= function
    | "none" -> return No_fill
    | "background" -> return Background_fill
    | "outline" -> return Outline_fill
    | _ -> error

let fill_type_expr = field "type" fill_type_atom

type fill =
  {
    fill_type: fill_type option
  ; kolor: kolor option
  }

let fill_expr: fill decoder =
  field "fill"
  (fields ~default:({fill_type=None; kolor=None}:fill)
     [
       ("type", fill_type_atom >>| (fun fill_t arg -> {arg with fill_type=Some fill_t}))
     ; ("color", kolor_args >>| (fun color arg -> {arg with kolor=Some color}))
     ]
  )

;;

let horizontal_justify_atom =
  atom >>=
    function
    | "left" ->
        return J_left
    | "right" ->
        return J_right
    | "center" ->
        return J_center
    | _ ->
        error

let vertical_justify_atom =
  atom >>=
    function
      | "top"    -> return J_top
      | "bottom" -> return J_bottom
      | "center" -> return J_center
      | _ ->
        error

type justification =
  {
    horiz: justify option
  ; vert: justify option
  }

let justify_args =
  let+ justif = tuple2 (maybe horizontal_justify_atom) (maybe vertical_justify_atom) in
  let horiz, vert = justif in
  {horiz; vert}

let justify_expr = field "justify" justify_args

;;

type font_def =
  {
    font: string option
  ; size: coord
  ; italic: bool
  ; bold: bool
  ; kolor: kolor option
  }

let size_expr = field "size" coords

let opt_to_bool = function
  | Some _ -> true
  | None -> false

(* Accept either a yes/no field form "(name yes|no)" (V8) or a bare tag atom "name" (V6/V7).
   Both produce a bool; bare tag always means true. *)
let bool_or_tag name =
  (yesno_expr name) |+> (tag name >>| fun _ -> true)

let font_args =
  let* font = maybe (field "face" (string ~escaped:false)) in
  let* size = size_expr in
  let* _line_spacing = maybe (field "line_spacing" float) in
  let* _thickness = maybe (field "thickness" skip) in
  let* italic = maybe (bool_or_tag "italic") >>| opt_to_bool in
  let* bold   = maybe (bool_or_tag "bold")   >>| opt_to_bool in
  let+ kolor = maybe kolor_expr in
  {font; size; italic; bold; kolor}

let font_expr = field "font" font_args
;;

let optional_hide_atom = (maybe @@ tag "hide") >>| opt_to_bool

type effects =
  {
    font: font_def
  ; justify: justification option
  ; hide: bool
  }

let effects_args =
  let* font = font_expr in
  let* justify = maybe justify_expr in
  let+ hide = maybe (bool_or_tag "hide") >>| opt_to_bool in
  (* If both are set, we take the first one, so we can use the "hide" tag in the font definition *)
  {font; justify; hide }

let effects_expr = field "effects" effects_args

let justify_of_justification j =
  let justify = Option.value j ~default:{horiz=None; vert=None} in
  Option.value ~default:J_center justify.horiz

let justify_of_effect e = justify_of_justification e.justify
let fontsize_of_effect e = let Coord (_x, y) = e.font.size in y

;;

let style_args =
  atom >>= fun s -> match s with
  | "default"
  | "solid"
  | "dash"
  | "dot"
  | "dash_dot"
  | "dash_dot_dot"  -> return s
  | _ -> error

let line_style_expr =
  field "type" style_args

let optional_line_style_expr = maybe @@ field "solid" line_style_expr

let width_expr = float_expr "width" >>| (fun w -> let width = wx_size w in Width width)
let optional_width_expr = maybe width_expr

type stroke =
  {
    width: width option
  ; kolor: kolor option
  ; style: string option
  }

let stroke_args =
  let* width = maybe width_expr in
  let* style = maybe line_style_expr in
  let+ kolor = maybe kolor_expr in
   {kolor; style; width}

let stroke_expr =
  field "stroke" stroke_args

;;

let optional_stroke_to_width = function
    | None
    | Some {width=None; _} -> 10
    | Some {width=Some (Width w); _} -> w

let default_width = 10

;;

type property =
  {
    name: string
  ; value: string
  ; id: int
  ; at : coord
  ; rot: int
  ; effects: effects option
  }

let property_build _name _ text _id at effects =
  match effects with
  | None
  | Some {hide=true; _} -> None
  | _ -> Some (
      let c = make_rel at in
      Text {text; c; s=Size 1})

let field_build {name; value; id; at; rot; effects} =
  let _ = name in
  let nb = id in
  let text = value in
  let co = at in
  let o =
    match rot with
    | 0 | 180 -> Orient_H
    | 90 | 270 -> Orient_V
    | _ -> raise Not_found in
  let s, j, stl, hide = match effects with
    | None -> Size default_width, J_left, NoStyle, false
    | Some ef ->
      let j = justify_of_effect ef in
      let Coord (_, y)= ef.font.size in
      let s = Size y in
      let style = match ef.font.italic, ef.font.bold with
        | true, false -> Italic
        | false, true -> Bold
        | true, true -> BoldItalic
        | false, false -> NoStyle
      in s, j, style, ef.hide in
  if hide then None else Some {nb; text; co; o; s; j; stl}

let property_args =
  let* name = string ~escaped:false in
  let* value = string ~escaped:false in
  let* id_opt = maybe (field "id" int) in
  let* at, rot = pin_at_coord_expr in
  let* _ = maybe (bool_or_tag "do_not_autoplace") in
  let+ effects = maybe effects_expr in
  let id = Option.value ~default:0 id_opt in
  {name; value; id; at; rot; effects}

let property_expr = field "property" property_args

;;

let text_gen_args =
  let* text = string ~escaped:false in
  let* _exclude_from_sim = maybe (yesno_expr "exclude_from_sim") in
  let* coords, rot = pin_at_coord_expr in
  let* effects = effects_expr in
  let+ _uuid = maybe uuid_expr in
  let Coord (size, _) = effects.font.size in
  let justify = justify_of_justification effects.justify in
  coords, text, Size size, rot, justify

let text_args = text_gen_args >>| (fun (coords, text, size, _rot, _justify) ->
  Text {c=make_rel coords; text; s=size })

let text_expr = field "text" text_args

;;

let polyline_args =
  let* pts = pts_expr in
  let* stroke = maybe stroke_expr in
  let* _fill = maybe fill_expr in
  let+ _uuid = maybe uuid_expr in
  (optional_stroke_to_width stroke), pts

let polyline_expr =
   field "polyline" polyline_args

;;

let start_point_expr =
  field "start" coords

let end_point_expr =
  field "end" coords

let rect_to_polyline ((Coord(xs, ys)), (Coord(xe, ye)), s, _) =
  let points = [RelCoord(xs, ys); RelCoord(xs, ye); RelCoord (xe, ye); RelCoord(xe, ys); RelCoord(xs, ys)] in
  let width = optional_stroke_to_width s
 in Polygon (width, points)

let rect_to_sheet_rec ((Coord(xs, ys)), (Coord(xe, ye)), _s, _) =
  (Coord(xs, ys), Coord(xe -xs, ye - ys))
let gen_rectangle_args =
  let* start_point = start_point_expr in
  let* end_point = end_point_expr in
  let* s = maybe stroke_expr in
  let* fill = maybe fill_expr in
  let+ _uuid = maybe uuid_expr in
  (start_point, end_point, s, fill)

let rectangle_prim_args = gen_rectangle_args >>| rect_to_polyline

let rectangle_args = gen_rectangle_args >>| rect_to_sheet_rec

let rectangle_expr = field
    "rectangle" rectangle_args

let rectangle_prim_expr = field
    "rectangle" rectangle_prim_args
;;

let pin_tag_expr s = field s (string ~escaped:false <*>effects_expr)

let pin_type_atom =
  atom >>|
  function
    | "bidirectional"
    | "passive"
    | "input"
    | "output"
    | "power_in"
    | "power_out"
    | "open_collector"
    | "open_drain"
    | "open_emitter"
    | "emitter_follower"
    | "source_follower"
    | "unconnected"
    | "no_connect"
    | "tri_state"
    | "unspecified" -> true
    | s -> failwith (Printf.sprintf "no match for pin type (%s)" s)

let pin_shape_atom =
  atom >>| function
    | "none"
    | "line"
    | "inverted"
    | "clock"
    | "inverted_clk"
    | "input_low"
    | "clock_low"
    | "falling_edge"
    | "non_logic" -> true
    | s -> failwith (Printf.sprintf "no match for pin shape (%s)" s)

let pin_alternate_args =
  field "alternate"
    (string ~escaped:false >>> pin_type_atom >>> pin_shape_atom >>| fun _ -> ())

let pin_args =
  let* _ = pin_type_atom in
  let* _ = pin_shape_atom in
  let* c, a = pin_at_coord_expr in
  let* s = field "length" float in
  let* _hide = maybe (bool_or_tag "hide") in
  let* (name_str, name_effect) = pin_tag_expr "name" in
  let* (number_str, number_effect) = pin_tag_expr "number" in
  let+ _alts = repeat_full_list pin_alternate_args in
  let contact = make_rel c in
  let orient = match a with
    | 0 -> P_R
    | 90 -> P_U
    | 180 -> P_L
    | 270 -> P_D
    | _ -> P_R (* Error *)
  in
  let length = Size (wx_size s) in
  let name =
    let Coord(x, _) = name_effect.font.size
    in name_str, (Size x)
  in
  let number =
    let Coord(x, _) = number_effect.font.size
    in number_str, (Size x)
  in
  Pin {name; number; length; contact; orient}

let pin_expr = field "pin" pin_args

;;

let angles_expr = field "angles" (float <*> float)

;;

let at_expr = field "at" coords

let radius_args =
  let* at = at_expr  >>| make_rel in
  let* length = length_expr in
  let+ angles = angles_expr in
  at, length, angles

let radius_expr =
  field "radius" radius_args

;;

let mid_point_expr =
  field "mid" coords >>| make_rel

let arc_center_radius_from_rel_points
    (RelCoord (ax, ay)) (RelCoord (bx, by)) (RelCoord (cx, cy)) =
  let ax = float_of_int ax and ay = float_of_int ay in
  let bx = float_of_int bx and by = float_of_int by in
  let cx = float_of_int cx and cy = float_of_int cy in
  let d = 2.0 *. ((bx -. ax) *. (cy -. by) -. (cx -. bx) *. (by -. ay)) in
  if Float.abs d < 1e-3 then None
  else
    let ab2 = bx *. bx +. by *. by -. ax *. ax -. ay *. ay in
    let bc2 = cx *. cx +. cy *. cy -. bx *. bx -. by *. by in
    let ux = (ab2 *. (cy -. by) -. bc2 *. (by -. ay)) /. d in
    let uy = ((bx -. ax) *. bc2 -. (cx -. bx) *. ab2) /. d in
    let r  = Float.sqrt ((ux -. ax) *. (ux -. ax) +. (uy -. ay) *. (uy -. ay)) in
    Some (RelCoord (Float.to_int (Float.round ux), Float.to_int (Float.round uy)),
          Float.to_int (Float.round r))

let arc_args =
  let* sp = start_point_expr >>| make_rel in
  let* mid = maybe mid_point_expr in
  let* ep = end_point_expr >>| make_rel in
  let* radius_group = maybe radius_expr in
  let* stroke = maybe stroke_expr in
  let+ _fill = maybe fill_expr in
  let width = optional_stroke_to_width stroke in
  let s = Size width in
  let center, radius =
    match radius_group with
    | Some (at, length, _angles) -> at, length
    | None ->
      (match mid with
       | Some mid_pt ->
         (match arc_center_radius_from_rel_points sp mid_pt ep with
          | Some (c, r) -> c, r
          | None -> failwith "arc: start, mid, end points are collinear")
       | None -> failwith "arc: either radius or mid point must be specified")
  in Arc { sp; ep; s; radius; center}


let arc_expr =
  field "arc" arc_args

;;

let bezier_args =
  let* pts = pts_expr in
  let* stroke = maybe stroke_expr in
  let+ _fill = maybe fill_expr in
  let points = make_rel_pts pts in
  Bezier (optional_stroke_to_width stroke, points)



let bezier_expr =
  field "gr_curve" bezier_args

;;

let center_expr =
  field "center" coords >>| make_rel

let circle_args =
  let* center = center_expr in
  let* radius = dist_expr "radius" in
  let* stroke = maybe stroke_expr in
  let+ _fill = maybe fill_expr in
  let width = optional_stroke_to_width stroke in
  Circle (width, {center; radius})

let circle_expr =
  field "circle" circle_args

;;

(* V8 schematic ellipse: (center x y) (major_radius r) (minor_radius r) (rotation_angle deg) *)
let ellipse_common_args =
  let* center = field "center" coords in
  let* major_radius = field "major_radius" float >>| (fun r -> Float.to_int (Float.round (r *. 100.0))) in
  let* minor_radius = field "minor_radius" float >>| (fun r -> Float.to_int (Float.round (r *. 100.0))) in
  let+ rotation_angle = field "rotation_angle" float >>| Float.round >>| int_of_float in
  (center, major_radius, minor_radius, rotation_angle)

let sch_ellipse_args =
  let* (center, ma, mi, rot) = ellipse_common_args in
  let* _stroke = maybe stroke_expr in
  let* _fill   = maybe fill_expr in
  let* _uuid   = maybe uuid_expr in
  let+ _locked = maybe (yesno_expr "locked") in
  (center, ma, mi, rot)

let sch_ellipse_arc_args =
  let* (center, ma, mi, rot) = ellipse_common_args in
  let* start_angle = field "start_angle" float >>| Float.round >>| int_of_float in
  let* end_angle   = field "end_angle" float >>| Float.round >>| int_of_float in
  let* _stroke = maybe stroke_expr in
  let* _fill   = maybe fill_expr in
  let* _uuid   = maybe uuid_expr in
  let+ _locked = maybe (yesno_expr "locked") in
  (center, ma, mi, rot, start_angle, end_angle)

;;

(* Library symbol ellipse primitive: (center x y) (major_radius r) (minor_radius r) (rotation_angle deg) *)
let ellipse_prim_args =
  let* center = center_expr in
  let* major_radius = dist_expr "major_radius" in
  let* minor_radius = dist_expr "minor_radius" in
  let* rotation_angle = field "rotation_angle" float >>| Float.round >>| int_of_float in
  let* stroke = maybe stroke_expr in
  let+ _fill = maybe fill_expr in
  let width = optional_stroke_to_width stroke in
  Ellipse (width, {center; major_radius; minor_radius; rotation_angle})

let ellipse_arc_prim_args =
  let* center = center_expr in
  let* major_radius = dist_expr "major_radius" in
  let* minor_radius = dist_expr "minor_radius" in
  let* rotation_angle = field "rotation_angle" float >>| Float.round >>| int_of_float in
  let* start_angle = field "start_angle" float >>| Float.round >>| int_of_float in
  let* end_angle   = field "end_angle" float >>| Float.round >>| int_of_float in
  let* stroke = maybe stroke_expr in
  let+ _fill = maybe fill_expr in
  let width = optional_stroke_to_width stroke in
  EllipseArc (width, {center; major_radius; minor_radius; rotation_angle; start_angle; end_angle})

;;

let primitive =
        variant
          [ ("polyline", polyline_args >>| (fun (s,l) -> Polygon (s, make_rel_pts l)))
          ; ("circle", circle_args)
          ; ("ellipse", ellipse_prim_args)
          ; ("ellipse_arc", ellipse_arc_prim_args)
          ; ("arc", arc_args)
          ; ("bezier", bezier_args)
          ; ("pin", pin_args)
          ; ("text", text_args)
          ; ("rectangle", rectangle_prim_args)
          ]

let extend_expr = string_expr "extends"

let unit_args =
  let* name = string ~escaped:false in
  let+ graphics = repeat_full_list primitive in
  match List.rev (String.split_on_char ~sep:'_' name) with
  | _style::part_str::_ -> let parts=int_of_string part_str in
    List.map ~f:(fun prim -> {parts; prim}) graphics
  | _ -> failwith ("malformed unit: " ^ name)

let unit_expr = field "symbol" unit_args

;;

let unit_peek expr = peek expr >>= function | Some _ -> return () | _ -> error

let pin_number_hide_expr = maybe_with_default false @@ ((field "pin_numbers" skip) >>= (fun _ -> return true))
let offset_expr = maybe_with_default 0 @@ dist_expr "offset"
let pin_names_expr = maybe_with_default true @@ (field "pin_names" (offset_expr <*> maybe skip) >>= fun _ -> return false)
let exclude_from_sim_expr = maybe (yesno_expr "exclude_from_sim")    
let in_bom_expr = yesno_expr "in_bom"
let on_board_expr = yesno_expr "on_board"

let symbol_args =
  let* name = string ~escaped:false in
  let* _pwr = maybe (field "power" no_more) in
  let* _extends = maybe extend_expr in
  let* hide_pin_numbers = pin_number_hide_expr in
  let* draw_pname = pin_names_expr in
  let* _exclude = exclude_from_sim_expr in
  let* _in_bom = in_bom_expr in
  let* _on_board = on_board_expr in
  let* _properties = repeat_list ~until:(unit_peek unit_expr) property_expr in
  let+ units = repeat_full_list (unit_expr |+> (field "embedded_fonts" (skip >>| fun _ -> []))) in
  let graph =  List.flatten units in
  {names=[name]; draw_pnum= not hide_pin_numbers; draw_pname; multi=false; graph}

let symbol_expr = field "symbol" symbol_args

;;

let lib_symbols_args = repeat_full_list symbol_expr

let lib_symbols_expr = field "lib_symbols" lib_symbols_args

;;

let junction_args =
  let* position = at_expr in
  let* _diameter = field "diameter" float in
  let* _color = kolor_expr in
  let+ _uuid = maybe uuid_expr
  in position

let junction_expr = field "junction" junction_args

;;

let no_connect_args =
  let* position = at_expr in
  let+ _uuid = maybe uuid_expr in
  position

let no_connect_expr = field "no_connect" no_connect_args

;;

let bus_entry_args =
  let* position = at_expr in
  let* size = field "size" (tuple2 float float) >>| (fun (s1, s2) -> mm_size s1, mm_size s2) in
  let* _stroke = stroke_expr in
  let+ _uuid = maybe uuid_expr in
  position, size

  let bus_entry_expr = field "bus_entry" bus_entry_args

;;

let bus_wire_args =
  let* pts = pts_expr in
  let* _stroke = stroke_expr in
  let+ _uuid = uuid_expr in
  pts

let bus_expr = field "bus" bus_wire_args
let wire_expr = field "wire" bus_wire_args

;;
;;

let label_args =
  let* text = string ~escaped:false in
  let* _exclude_from_sim = maybe (yesno_expr "exclude_from_sim") in
  let* coords, rot = pin_at_coord_expr in
  let* _autoplaced = maybe (field "fields_autoplaced" skip_all) in
  let* effects = effects_expr in
  let+ _uuid = maybe uuid_expr in
  let Coord (size, _) = effects.font.size in
  (coords, rot, text, Size size, justify_of_justification effects.justify)

;;

let shape_args = string ~escaped:false >>| (function
    | "input" -> InputPort
    | "output" -> OutputPort
    | "bidirectional" -> BiDiPort
    | "tri_state" -> ThreeStatePort
    | "passive" -> ThreeStatePort
    | s -> failwith ("unknown shape " ^ s)
  )

;;

let hierarchical_label_args =
  let* text = string ~escaped:false in
  let* shape = field "shape" shape_args in
  let* coords, rot = pin_at_coord_expr in
  let* _autoplace = maybe (field "fields_autoplaced" skip_all) in
  let* effects = effects_expr in
  let* _uuid = maybe uuid_expr in
  let+ _prop = maybe (repeat_full_list property_expr) in
  let Coord (size, _) = effects.font.size in
  (coords, rot, text, Size size, shape, justify_of_justification effects.justify)

let global_label_expr = field "global_label" hierarchical_label_args

;;

let sch_pin_args =
  let* name = string ~escaped:false in
  let+ _id = uuid_expr in
  name

let sch_pin_expr = field "pin" sch_pin_args
;;
(*
let path_instance_args =
  let* _path_name = string ~escaped:false in
  let* _reference = string_expr "reference" in
  let* _unit_v = int_expr "unit" in
  let* _value = string_expr "value" in
  let+ _footprint = string_expr "footprint" in
  ()
*)

let path_instance_args =
  let* _path_name = string ~escaped:false in
  let* _reference = string_expr "reference" in
  let+ _unit_v = int_expr "unit" in
  ()

let path_instance_expr = field "path" path_instance_args

let project_args =
  let* _project_name = string ~escaped:false in
  let+ _paths = (repeat1_full_list path_instance_expr) in
  ()

let project_instance_expr = field "project" project_args

let instances_args = repeat1_full_list project_instance_expr >>| (fun _ -> 0 )

let instances_expr = field "instances" instances_args

type lib_sym = {lib_id: string; lib_name: string option; pos: coord; rot: int; unit_nr: int; properties: property list; mirror_x: bool; mirror_y: bool}

let sch_symbol_args =
  let* lib_name = maybe (string_expr "lib_name") in
  let* lib_id =  string_expr "lib_id" in
  let+ lib_sym = fields
      ~default: {lib_id; lib_name; pos= Coord(0,0); rot=0; unit_nr=1; properties=[]; mirror_x=false; mirror_y=false}
      [
        ("at", pin_at_coord_args >>| (fun (pos, rot) args -> {args with pos; rot}))
      ; ("unit", int  >>| (fun unit_nr args -> {args with unit_nr}))
      ; ("body_style", int  >>| (fun _ args -> args))
      ; ("in_bom", atom >>| (fun _ args -> args))
      ; ("on_board", atom >>| (fun _ args -> args))
      ; ("in_pos_files", atom >>| (fun _ args -> args))
      ; ("exclude_from_sim", atom >>| (fun _ args -> args))
      ; ("fields_autoplaced", maybe atom >>| (fun _ args -> args))
      ; ("dnp", atom >>| (fun _ args -> args))
      ; ("mirror", atom >>| (fun axis args -> match axis with
          | "x" -> {args with mirror_x=true}
          | "y" -> {args with mirror_y=true}
          | _ -> failwith "unknown mirror axis"))
      ; ("uuid", atom >>|  (fun _ args -> args))
      ; ("property", property_args >>| (fun prop args -> {args with properties=prop::args.properties}))
      ; ("pin", sch_pin_args >>| (fun _ args -> args))
      ; ("instances", instances_args >>| (fun _ args -> args))
      ]
  in lib_sym

let sch_symbol_expr = field "symbol" sch_symbol_args

;;
let parse_list ?(cond = fun _ -> true) form s =
  let stream = Scanf.Scanning.from_string s in
  let rec do_parse acc =
    try
      let new_val = Scanf.bscanf stream form (fun x -> x) in
      if cond new_val then do_parse (new_val :: acc) else acc
    with
    | Scanf.Scan_failure _ ->
        acc
    | End_of_file ->
        acc
  in
  do_parse []

let image_args = fields
    ~default: {pos=None; scale=None; data=Some (Buffer.create 1000)}
    [ ("at", pin_at_coord_args >>| (fun (apos, _) (args:bitmapContext) -> {args with pos=(Some apos)}))
    ; ("uuid", atom >>| (fun _ args -> args))
    ; ("scale", float >>| (fun scale args -> {args with scale=Some scale}))
    ; ("data", repeat1_full_list (string ~escaped:false) >>| (fun (strings: string list) args ->
           let d = Buffer.create 1000 in
           List.iter ~f:(Buffer.add_string d) strings;
           match Base64.decode (Buffer.contents d) with
           | Ok st -> let databuf = Buffer.create (String.length st) in Buffer.add_string databuf st; {args with data=(Some databuf)}
           | Error `Msg msg -> Format.print_string msg; args
         ))
    ]

;;

let sheet_pin_args =
  let* name= string ~escaped:false in
  let* port_type = shape_args in
  let* pos, _angle = pin_at_coord_expr in
  let* _uuid = maybe uuid_expr in
  let* effects = effects_expr in
  let+ _uuid = maybe uuid_expr in
  let justif = justify_of_effect effects in
  let sz = fontsize_of_effect effects in
  (name, port_type, justif, pos, (Size sz))

let sheet_pin_expr = field "pin" sheet_pin_args

let sheet_path_instance_args =
  let* _path_name = string ~escaped:false in
  let+ _page = field "page" (string ~escaped:false) in
  ()

let sheet_path_instance_expr = field "path" sheet_path_instance_args

let sheet_project_args =
  let* _project_name = string ~escaped:false in
  let+ _paths = (repeat1_full_list sheet_path_instance_expr) in
  ()

let sheet_project_instance_expr = field "project" sheet_project_args

let sheet_instances_args = repeat1_full_list sheet_project_instance_expr >>| (fun _ -> 0 )

let sheet_instances_expr = field "instances" sheet_instances_args


let sheet_args =
  let* at, _angle = pin_at_coord_expr in
  let* size = size_expr in
  let* _fields_autoplaced = maybe (field "fields_autoplaced" no_more) in
  let* _stroke = maybe stroke_expr in
  let* _fill = maybe fill_expr in
  let* _uuid = maybe uuid_expr in
  let* properties = repeat_list ~until:(unit_peek ( (sheet_pin_expr >>| (fun _ -> ())) |+> (sheet_instances_expr >>| (fun _ -> ())))) property_expr in
  let* hierarchical_pins = repeat_list ~until:(unit_peek sheet_instances_expr) sheet_pin_expr in
  let+ _instances = sheet_instances_expr in
  (at, size, properties, hierarchical_pins)

;;

let title_block_args =
  let* title = field "title" (string ~escaped:false) in
  let* date = field "date" (string ~escaped:false) in
  let* rev = field "rev" (string ~escaped:false) in
  let* company = field "company" (string ~escaped:false) in
  let+ comments = repeat_full_list (field "comment" (int <*> string ~escaped:false)) in
  (title, date, rev, company, comments)

(* V8: all title-block fields are optional *)
let title_block_v8_args =
  let* title   = maybe (field "title"   (string ~escaped:false)) >>| Option.value ~default:"" in
  let* date    = maybe (field "date"    (string ~escaped:false)) >>| Option.value ~default:"" in
  let* rev     = maybe (field "rev"     (string ~escaped:false)) >>| Option.value ~default:"" in
  let* company = maybe (field "company" (string ~escaped:false)) >>| Option.value ~default:"" in
  let+ comments = repeat_full_list (field "comment" (int <*> string ~escaped:false)) in
  (title, date, rev, company, comments)

(* V8 sheet instances: path entries may have variant sub-blocks, which we skip *)
let sheet_path_instance_args_v8 =
  let* _path_name = string ~escaped:false in
  let* _page = field "page" (string ~escaped:false) in
  let+ _ = skip_all in
  ()

let sheet_path_instance_expr_v8 = field "path" sheet_path_instance_args_v8

let sheet_project_args_v8 =
  let* _project_name = string ~escaped:false in
  let+ _paths = repeat1_full_list sheet_path_instance_expr_v8 in
  ()

let sheet_project_instance_expr_v8 = field "project" sheet_project_args_v8

let sheet_instances_args_v8 = repeat1_full_list sheet_project_instance_expr_v8 >>| (fun _ -> 0)

let sheet_instances_expr_v8 = field "instances" sheet_instances_args_v8

(* V8 sheet: adds exclude_from_sim/in_bom/on_board/dnp/locked; instances is optional *)
let sheet_args_v8 =
  let* at, _angle = pin_at_coord_expr in
  let* size = size_expr in
  let* _excl     = maybe (yesno_expr "exclude_from_sim") in
  let* _in_bom   = maybe (yesno_expr "in_bom") in
  let* _on_board = maybe (yesno_expr "on_board") in
  let* _dnp      = maybe (yesno_expr "dnp") in
  let* _locked   = maybe (yesno_expr "locked") in
  let* _fields_autoplaced = maybe (field "fields_autoplaced" skip_all) in
  let* _stroke = maybe stroke_expr in
  let* _fill   = maybe fill_expr in
  let* _uuid   = maybe uuid_expr in
  let* properties =
    repeat_list
      ~until:(unit_peek ((sheet_pin_expr >>| (fun _ -> ())) |+>
                         (sheet_instances_expr_v8 >>| (fun _ -> ()))))
      property_expr in
  let* hierarchical_pins =
    repeat_list ~until:(unit_peek sheet_instances_expr_v8) sheet_pin_expr in
  let+ _instances = maybe sheet_instances_expr_v8 in
  (at, size, properties, hierarchical_pins)

(* ── V8 schematic-level shape decoders ─────────────────────────────── *)

(* Compute circumscribed circle center and radius from three arc points.
   Returns None when the three points are collinear (degenerate arc). *)
let sch_arc_center_radius (Coord (ax, ay)) (Coord (bx, by)) (Coord (cx, cy)) =
  let ax = float_of_int ax and ay = float_of_int ay in
  let bx = float_of_int bx and by = float_of_int by in
  let cx = float_of_int cx and cy = float_of_int cy in
  let d = 2.0 *. ((bx -. ax) *. (cy -. by) -. (cx -. bx) *. (by -. ay)) in
  if Float.abs d < 1e-3 then None
  else
    let ab2 = bx *. bx +. by *. by -. ax *. ax -. ay *. ay in
    let bc2 = cx *. cx +. cy *. cy -. bx *. bx -. by *. by in
    let ux = (ab2 *. (cy -. by) -. bc2 *. (by -. ay)) /. d in
    let uy = ((bx -. ax) *. bc2 -. (cx -. bx) *. ab2) /. d in
    let r  = Float.sqrt ((ux -. ax) *. (ux -. ax) +. (uy -. ay) *. (uy -. ay)) in
    Some (Coord (Float.to_int (Float.round ux), Float.to_int (Float.round uy)),
          Float.to_int (Float.round r))

(* V8 schematic arc: (start ...) (mid ...) (end ...) — three points on arc *)
let sch_arc_args =
  let* start_pt = start_point_expr in
  let* mid_pt   = field "mid" coords in
  let* end_pt   = end_point_expr in
  let* _stroke  = maybe stroke_expr in
  let* _fill    = maybe fill_expr in
  let* _uuid    = maybe uuid_expr in
  let+ _locked  = maybe (yesno_expr "locked") in
  match sch_arc_center_radius start_pt mid_pt end_pt with
  | Some (center, radius) -> Some (center, start_pt, end_pt, radius)
  | None -> None

(* V8 schematic circle: (center x y) (radius r) *)
let sch_circle_args =
  let* center = field "center" coords in
  let* radius = field "radius" float >>| (fun r -> Float.to_int (Float.round (r *. 100.0))) in
  let* _stroke = maybe stroke_expr in
  let* _fill   = maybe fill_expr in
  let* _uuid   = maybe uuid_expr in
  let+ _locked = maybe (yesno_expr "locked") in
  (center, radius)

(* Sample a cubic Bézier at n+1 evenly-spaced t values in [0,1].
   B(t) = (1-t)³P0 + 3(1-t)²tP1 + 3(1-t)t²P2 + t³P3 *)
let sch_bezier_sample p0 p1 p2 p3 n =
  let fx (Coord (x, _)) = float_of_int x
  and fy (Coord (_, y)) = float_of_int y in
  let cubic t =
    let mt = 1.0 -. t in
    let x = mt*.mt*.mt*.(fx p0) +. 3.*.mt*.mt*.t*.(fx p1)
           +. 3.*.mt*.t*.t*.(fx p2) +. t*.t*.t*.(fx p3) in
    let y = mt*.mt*.mt*.(fy p0) +. 3.*.mt*.mt*.t*.(fy p1)
           +. 3.*.mt*.t*.t*.(fy p2) +. t*.t*.t*.(fy p3) in
    Coord (Float.to_int (Float.round x), Float.to_int (Float.round y))
  in
  List.init ~len:(n + 1) ~f:(fun i -> cubic (float_of_int i /. float_of_int n))

(* V8 schematic bezier: (pts (xy ...) ...) — 4 control points *)
let sch_bezier_args =
  let* pts     = pts_expr in
  let* _stroke = maybe stroke_expr in
  let* _fill   = maybe fill_expr in
  let* _uuid   = maybe uuid_expr in
  let+ _locked = maybe (yesno_expr "locked") in
  match pts with
  | [p0; p1; p2; p3] -> sch_bezier_sample p0 p1 p2 p3 16
  | _ -> pts   (* fallback: draw the control polygon *)

(* V8 text_box position: either (at + size) or legacy (start + end) *)
let text_box_position_args =
  (let* at, _angle = pin_at_coord_expr in
   let+ Coord (w, h) = size_expr in
   let Coord (x, y) = at in
   (Coord (x, y), Coord (w, h)))
  |+>
  (let* s = start_point_expr in
   let+ Coord (xe, ye) = end_point_expr in
   let Coord (xs, ys) = s in
   (s, Coord (xe - xs, ye - ys)))

(* V8 schematic text_box *)
let sch_text_box_args =
  let* text    = string ~escaped:false in
  let* _excl   = maybe (yesno_expr "exclude_from_sim") in
  let* corner, dim = text_box_position_args in
  let* _marg   = maybe (field "margins" skip_all) in
  let* _stroke = maybe stroke_expr in
  let* _fill   = maybe fill_expr in
  let* effects = maybe effects_expr in
  let* _uuid   = maybe uuid_expr in
  let+ _locked = maybe (yesno_expr "locked") in
  (corner, dim, text, effects)

(* V8 rule_area: flags + embedded polyline *)
let sch_rule_area_args =
  let* _locked   = maybe (yesno_expr "locked") in
  let* _excl     = maybe (yesno_expr "exclude_from_sim") in
  let* _in_bom   = maybe (yesno_expr "in_bom") in
  let* _on_board = maybe (yesno_expr "on_board") in
  let* _dnp      = maybe (yesno_expr "dnp") in
  let+ (_w, pts) = polyline_expr in
  pts
