open KicadDefs
module Sigs = KicadSch_sigs
open Sigs
open KicadLib_sigs
module Decode = Sexp_decode.Make(Base.Sexp)
open Decode
open! StdLabels

(* let mm_size x = int_of_float (x *. 1000.) *)
let wx_size x = int_of_float (x *. (1000. /. 25.4))

let coords = float <*> float >>| (fun (x, y) -> Coord (wx_size x, wx_size y))


let mm_size x = int_of_float (x *. 1000.)
let wx_size x = int_of_float (x *. (1000. /. 25.4))


let dist_expr s =
  field s  float >>| (fun s -> wx_size s)
let length_expr =
  dist_expr "length"

type kolor = {red: int; green: int; blue: int; alpha: int}
type width = Width of int

let kolor_args =
  let+ colors = tuple4 int  int  int  int in
  let red, green, blue, alpha = colors in
  {red; green; blue; alpha}

let kolor_expr = field "color" kolor_args


let gen_uuid_expr s =
  field s (string ~escaped:false) >>| (fun s -> match Uuidm.of_string s with Some u -> return u | None -> failwith "Not a uuid")

let uuid_expr = gen_uuid_expr "uuid"
let page_UUID_expr = gen_uuid_expr "page"
let page_expr = field "page" (string ~escaped:false)
;;

let paper_size = variant
    [ ("A5" , return (Coord ((mm_size 210.), (mm_size 148.))))
    ; ("A4" , return (Coord ((mm_size 297.), (mm_size 210.))))
    ; ("A5" , return (Coord((mm_size  210.  ), (mm_size 148.  ))))
    ; ("A4" , return (Coord((mm_size  297.  ), (mm_size 210.  ))))
    ; ("A3" , return (Coord((mm_size  420.  ), (mm_size 297.  ))))
    ; ("A2" , return (Coord((mm_size  594.  ), (mm_size 420.  ))))
    ; ("A1" , return (Coord((mm_size  841.  ), (mm_size 594.  ))))
    ; ("A0" , return (Coord((mm_size  1189. ), (mm_size 841.  ))))
    ; ("A"  , return (Coord((wx_size  11000.), (wx_size  8500.))))
    ; ("B"  , return (Coord((wx_size  17000.), (wx_size 11000.))))
    ; ("C"  , return (Coord((wx_size  22000.), (wx_size 17000.))))
    ; ("D"  , return (Coord((wx_size  34000.), (wx_size 22000.))))
    ; ("E"  , return (Coord((wx_size  44000.), (wx_size 34000.))))
    ; ("GERBER",  coords)
    ; ("User",  coords)
    ; ("USLetter", return (Coord((wx_size  11000.), (wx_size 8500.))))
    ; ("USLegal" , return (Coord((wx_size  14000.), (wx_size 8500.))))
    ; ("USLedger", return (Coord((wx_size  17000.), (wx_size 11000.))))
    ] <*> option (tag "portrait") >>|(fun (Coord(x, y) as c, p) -> match p with Some _ -> c | None ->  (Coord(y, x)))

;;
let paper_expr =
  field "paper" paper_size
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

let yesno_expr s =
  field s atom >>= (function | "yes" -> return true | "no" -> return false | _ -> error)

;;

let pin_at_coord_expr =
  let+ at_coords = field "at" @@ tuple3 float float (maybe int) in
  let x, y, _ = at_coords in
  Coord (wx_size x, wx_size y)

let pin_at_coord2_expr =
  let+ at_coords = field "at" @@ tuple3 float float (maybe int) in
  let x, y, angle = at_coords in
  Coord (wx_size x, wx_size y), angle

;;

type fill_type =
  | No_fill
  | Background_fill
  | Outline_fill

let fill_type_atom = atom >>= fun s -> match s with
    | "none" -> return No_fill
    | "background" -> return Background_fill
    | "outline" -> return Outline_fill
    | _ -> error

let fill_type_expr = field "type" fill_type_atom

type fill =
  {
    fill_type: fill_type
  ; kolor: kolor option
  }

let fill_expr =
  field "fill" (fill_type_expr <*> maybe kolor_expr >>| (fun (fill_type, kolor) -> {fill_type; kolor}))

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
    horiz: justify
  ; vert: justify
  }

let justify_args =
  let+ justif = tuple2 horizontal_justify_atom vertical_justify_atom in
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

let italic_atom = tag "italic" >>| fun _ -> true
let bold_atom = tag "bold" >>| fun _ -> true

let size_expr = field "size" coords

let opt_to_bool = function
  | Some _ -> true
  | None -> false

let font_args =
  let* font = maybe @@ string ~escaped:true in
  let* size = size_expr in
  let* italic_opt = maybe italic_atom in
  let* bold_opt = maybe bold_atom in
  let+ kolor = maybe kolor_expr in
  let italic = opt_to_bool italic_opt in
  let bold = opt_to_bool bold_opt in
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
  let+ hide = optional_hide_atom in
  {font; justify; hide}

let effects_expr = field "effects" effects_args

;;

let style_args =
  atom >>= fun s -> match s with
   | "solid"
   | "dash"
   | "dot"
   | "dash-dot" -> return s
   | _ -> error



let line_style_expr =
  field "style" style_args

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
  let* style = maybe line_style_expr in
  let* width = maybe width_expr in
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
  ; effects: effects option
  }

let property_build _name _ text _id at effects =
  match effects with
  | None
  | Some {hide=true; _} -> None
  | _ -> Some (
      let c = make_rel at in
      Text {text; c; s=Size 1})

let property_args =
  let* name = string ~escaped:true in
  let* value = string ~escaped:true in
  let* id = field "id" int in
  let* at = pin_at_coord_expr in
  let+ effects = maybe effects_expr in
  {name; value; id; at; effects}

let property_expr = field "property" property_args

;;

let text_args =
  let* text = string ~escaped:true in
  let* coords = pin_at_coord_expr in
  let+ effects = effects_expr in
  let Coord (size, _) = effects.font.size in
  Text {c=make_rel coords; text; s=Size size }

let text_expr = field "text" text_args

;;

let polyline_args =
  let* pts = pts_expr in
  let* stroke = maybe stroke_expr in
  let+ _fill = maybe fill_expr in
  let points = make_rel_pts pts in
  Polygon((optional_stroke_to_width stroke), points)

let polyline_expr =
   field "polyline" polyline_args
;;


;;

let start_point_expr =
  field "start" coords >>| make_rel

let end_point_expr =
  field "end" coords >>| make_rel

let rect_to_polyline (RelCoord(xs, ys)) (RelCoord(xe, ye)) s _ =
  let points = [RelCoord(xs, ys); RelCoord(xs, ye); RelCoord (xe, ye); RelCoord(xe, ys); RelCoord(xs, ys)] in
  let width = optional_stroke_to_width s
 in
  Polygon (width, points)

let rectangle_args =
  let* start_point = start_point_expr in
  let* end_point = end_point_expr in
  let* s = maybe stroke_expr in
  let+ fill = maybe fill_expr in
  rect_to_polyline start_point end_point s fill


let rectangle_expr = field
    "rectangle" rectangle_args
;;

let pin_tag_expr s = field s (string ~escaped:true <*>effects_expr)

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
    | "emitter_follower"
    | "source_follower"
    | "unconnected"
    | "tristate"
    | "unspecified" -> true
    | s -> failwith (Printf.sprintf "no match for pin atom (%s)" s)

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

let pin_args =
  let* _ = pin_type_atom in
  let* _ = pin_shape_atom in
  let* c, a = pin_at_coord2_expr in
  let* s = field "length" float in
  let* (name_str, name_effect) = pin_tag_expr "name" in
  let+ (number_str, number_effect) = pin_tag_expr "number" in
  let contact = make_rel c in
  let orient = match a with
    | Some 0 -> P_R
    | Some 90 -> P_U
    | Some 180 -> P_L
    | Some 270 -> P_D
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

let arc_args =
  let* sp = start_point_expr in
  let* _mid_point = maybe mid_point_expr in
  let* ep = end_point_expr in
  let* radius_group = maybe radius_expr in
  let* stroke = maybe stroke_expr in
  let+ _fill = maybe fill_expr in
  let width = optional_stroke_to_width stroke in
  let s = Size width in
  let center, radius =
    match radius_group with
    | None -> RelCoord (0, 0), 0 (* TODO: use mid_point to compute center and radius: https://github.com/KiCad/kicad-source-mirror/blob/f353fc448b3c8e86818b52f66020f31d671ae048/libs/kimath/src/trigo.cpp#L386 *)
     | Some (at, length, _angles) -> at, length
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

let primitive =
        variant
          [ ("polyline", polyline_args)
          ; ("circle", circle_args)
          ; ("arc", arc_args)
          ; ("bezier", bezier_args)
          ; ("pin", pin_args)
          ; ("text", text_args)
          ; ("rectangle", rectangle_args)
          ]

let extend_expr = string_expr "extends"

let unit_args =
  let* name = string ~escaped:true in
  let+ graphics = repeat_full_list primitive in
  match List.rev (String.split_on_char ~sep:'_' name) with
  | _style::part_str::_ -> let parts=int_of_string part_str in
    List.map ~f:(fun prim -> {parts; prim}) graphics
  | _ -> failwith "malformed unit"

let unit_expr = field "symbol" unit_args

;;

let unit_peek expr = peek expr >>= function | Some _ -> return () | _ -> error

let pin_number_hide_expr = maybe_with_default false @@ ((field "pin_numbers" skip) >>= (fun _ -> return true))
let offset_expr = maybe_with_default 0 @@ dist_expr "offset"
let pin_names_expr = maybe_with_default true @@ ((offset_expr <*> skip) >>= fun _ -> return false)

let in_bom_expr = yesno_expr "in_bom"
let on_board_expr = yesno_expr "on_board"

let symbol_args =
  let* name = string ~escaped:true in
  let* _extends = maybe extend_expr in
  let* hide_pin_numbers = pin_number_hide_expr in
  let* draw_pname = pin_names_expr in
  let* _in_bom = in_bom_expr in
  let* _on_board = on_board_expr in
  let* _properties = repeat_list ~until:(unit_peek unit_expr) property_expr in
  let+ units = repeat_full_list unit_expr in
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
  let* size = field "size" (tuple2 float float) in
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
(*
let shape_args = variant
    [ ("input", InputPort)
    ; ("output", OutputPort)
    ; ("bidirectional", BiDiPort)
    ; ("tri_state", ThreeStatePort)
    ; ("passive", ThreeStatePort)
    ]

let global_hierarchical_label_args =
  let* text = string ~escaped:true in
  let* shape = field "shape" shape_args
  let* coords = pin_at_coord_expr in
  let+ effects = effects_expr in
  let Coord (size, _) = effects.font.size in
*)
