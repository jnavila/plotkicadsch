open KicadDefs
module Sigs = KicadSch_sigs
open Sigs
open KicadLib_sigs
(* open KicadLib_sigs *)
open Angstrom
open! StdLabels

type kolor = {red: int; green: int; blue: int; alpha: int}
type width = Width of int

let mm_size x = int_of_float (x *. 1000.)
let wx_size x = int_of_float (x *. (1000. /. 25.4))

let ws = skip_while (function
  | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
  | _ -> false)

let space = skip_many1 (choice
    [
      char '\x20'
    ; char '\x0a'
    ; char '\x0d'
    ; char '\x09'
    ])

let header s = string s <* space
let keyword s = ws *> string s <* ws

let optional p = option None (ws*> p >>| (fun r -> Some r))

(* This is mainly for defining the atoms and combinations of atoms *)

let parens p =  ws *> char '(' *> ws *> p <* ws <* char ')'<* ws
let integer_atom = take_while1 (function '0' .. '9' -> true | _ -> false ) >>| int_of_string
let floating_point = take_while1 (function '0' .. '9' | '.' | '-' -> true | _ -> false ) >>| float_of_string
let coords =  (fun x _ y -> Coord (wx_size x, wx_size y))  <$> floating_point <*> space <*> floating_point
let escaped_string_atom = char '"' *> many ((char '\\' *> take 1) <|> take_while1 (function '"'|'\\' -> false |  _ -> true))  <* char '"' >>| String.concat ~sep:""

let expression k p =
  let content =  header k *> p <?> k  in
  parens content

(* expr is for expressions which are s-expr expressions ( inside parenthesis ) *)

let color_expr =
  expression "color" ((fun red _ green _ blue _ alpha -> {red; green; blue; alpha}) <$> integer_atom <*> space <*>integer_atom <*> space <*> integer_atom <*> space <*> integer_atom)

let string_expr s =
  expression s escaped_string_atom

let int_expr s =
  expression s integer_atom

let float_expr s =
  expression s floating_point

let yesno_expr s =
  expression s  (choice
                   [
                     string "yes" *> return true
                   ; string "no"   *> return false
                   ])

let dist_expr s =
  expression s  (floating_point >>| (fun s -> wx_size s))
let length_expr =
  dist_expr "length"

let pin_at_expr = expression "at" ((fun c _ a -> c,a) <$> coords <*> space <*> option 0 integer_atom)
let pin_at_coord_expr = pin_at_expr >>| (fun (c, _a) -> c)

;;

let version_expr = string_expr "version"
let generator_expr = string_expr "generator"

let gen_uuid_expr s = string_expr s >>= (fun s -> match Uuidm.of_string s with Some u -> return u | None -> fail "Not a uuid")
let uuid_expr = gen_uuid_expr "uuid"
let page_UUID_expr = gen_uuid_expr "page"
let page_expr = string_expr "page"
;;

let paper_size: coord t = choice ~failure_msg:"unknown paper format"
    [
      string "A5" *> return (Coord ((mm_size 210.), (mm_size 148.)))
    ; string "A4" *> return (Coord ((mm_size 297.), (mm_size 210.)))
    ; string "A5" *> return (Coord((mm_size  210.  ), (mm_size 148.  )))
    ; string "A4" *> return (Coord((mm_size  297.  ), (mm_size 210.  )))
    ; string "A3" *> return (Coord((mm_size  420.  ), (mm_size 297.  )))
    ; string "A2" *> return (Coord((mm_size  594.  ), (mm_size 420.  )))
    ; string "A1" *> return (Coord((mm_size  841.  ), (mm_size 594.  )))
    ; string "A0" *> return (Coord((mm_size  1189. ), (mm_size 841.  )))
    ; string "A"  *> return (Coord((wx_size  11000.), (wx_size  8500.)))
    ; string "B"  *> return (Coord((wx_size  17000.), (wx_size 11000.)))
    ; string "C"  *> return (Coord((wx_size  22000.), (wx_size 17000.)))
    ; string "D"  *> return (Coord((wx_size  34000.), (wx_size 22000.)))
    ; string "E"  *> return (Coord((wx_size  44000.), (wx_size 34000.)))
    ; string "GERBER" *> coords
    ; string "User" *> coords
    ; string "USLetter"*> return (Coord((wx_size  11000.), (wx_size 8500.)))
    ; string "USLegal" *> return (Coord((wx_size  14000.), (wx_size 8500.)))
    ; string "USLedger"*> return (Coord((wx_size  17000.), (wx_size 11000.)))
    ] >>= (fun (Coord (x, y) as c) -> option c ( ws *> string "portrait" *> return (Coord(y, x))))
;;

let paper_expr =
  expression "paper"  (char '"' *> paper_size <* char '"')
;;

let xy_expr =
  expression "xy" coords

let pts_expr =
  expression "pts" (many1 xy_expr)

let make_rel (Coord(x,y)) = RelCoord(x,y)
let make_rel_pts pts =
  List.map ~f:make_rel pts
;;

let line_style_expr =
  expression "style" (choice
                        [
                          string "solid"
                        ; string "dash"
                        ; string "dot"
                        ; string "dash-dot"
                        ])

let optional_line_style_expr = option "solid" line_style_expr

let width_expr = float_expr "width" >>| (fun w -> let width = wx_size w in Width width)
let optional_width_expr = ws*> optional width_expr
;;

type stroke =
  {
    width: width option
  ; kolor: kolor option
  ; style: string
  }

let stroke_expr =
  expression "stroke" ((fun width style kolor -> {width; style; kolor}) <$> optional_width_expr <*> optional_line_style_expr <*> optional color_expr)

let optional_stroke_to_width = function
    | None
    | Some {width=None; _} -> 1
    | Some {width=Some (Width w); _} -> w

let default_width = 10

;;

type fill_type =
  | No_fill
  | Background_fill
  | Outline_fill

let fill_type_atom = choice
    [
      keyword "none" *> return No_fill
    ; keyword "background" *> return Background_fill
    ; keyword "outline" *> return Outline_fill
    ]

let fill_type_expr = expression "type" fill_type_atom

type fill =
  {
    fill_type: fill_type
  ; kolor: kolor option
  }

let fill_expr =
  expression "fill" ((fun fill_type kolor -> {fill_type; kolor}) <$> fill_type_expr <*> optional color_expr)
;;

(* Library description *)

let offset_expr = dist_expr "offset"
let optional_offset_expr = option 0 offset_expr

let pin_numbers_expr =
  expression "pin_numbers" ((fun _ -> true) <$> (keyword "hide"))

let optional_pin_number_expr =
  option false pin_numbers_expr

let pin_names_expr =
  expression "pin_names" ((fun offset _ -> offset) <$> optional_offset_expr <*> (keyword "hide"))

let optional_pin_names_expr =
  optional pin_names_expr

let polyline_expr =
  expression "polyline"
    ((fun pts stroke _fill -> let points = make_rel_pts pts in Polygon ((optional_stroke_to_width stroke), points))
     <$> pts_expr <*> optional stroke_expr <*> optional fill_expr)

;;

let start_point_expr =
  expression "start" coords >>| make_rel

let end_point_expr =
  expression "end" coords >>| make_rel

let rect_to_polyline (RelCoord(xs, ys)) (RelCoord(xe, ye)) s _ =
  let points = [RelCoord(xs, ys); RelCoord(xs, ye); RelCoord (xe, ye); RelCoord(xe, ys); RelCoord(xs, ys)] in
  let width = optional_stroke_to_width s
 in
  Polygon (width, points)

let rectangle_expr =
  expression "rectangle"
    (rect_to_polyline <$>
     start_point_expr <*> end_point_expr <*> optional stroke_expr <*> optional fill_expr)
;;

let center_expr =
  expression "center" coords >>| make_rel

let center_build center radius s _f =
  let width = optional_stroke_to_width s in
  Circle (width, {center; radius})

let circle_expr =
  expression "circle"
    (center_build
     <$> center_expr <*> dist_expr "radius"
     <*> optional stroke_expr <*> optional fill_expr)
;;
(* The description of an arc is not clear from the specification or the code *)
let mid_point_expr =
  expression "mid" coords >>| make_rel

let angles_expr =
  expression "angles"
    ((fun st_rad_angle _  end_rad_angle -> st_rad_angle, end_rad_angle)
     <$> floating_point <*> space <*> floating_point)

let at_expr = expression "at" ( make_rel <$> coords)


let radius_expr =
  expression "radius" ((fun at length angles -> at, length, angles) <$>
                       at_expr <*> length_expr <*> angles_expr)

let arc_build  sp  _mid_point ep radius_group stroke _ =
  let width = optional_stroke_to_width stroke in
  let s = Size width in
  let center, radius =
   match radius_group with
         | None -> RelCoord (0, 0), 0 (* TODO: use mid_point to compute center and radius: https://github.com/KiCad/kicad-source-mirror/blob/f353fc448b3c8e86818b52f66020f31d671ae048/libs/kimath/src/trigo.cpp#L386 *)
     | Some (at, length, _angles) -> at, length
  in Arc { sp; ep; s; radius; center}

let arc_expr =
  expression "arc"
      (arc_build <$> start_point_expr <*> optional mid_point_expr <*> end_point_expr <*> optional radius_expr <*> optional stroke_expr <*> optional fill_expr) 

;;

let bezier_build pts stroke _fill =
  let points = make_rel_pts pts in
  Bezier (optional_stroke_to_width stroke, points)

let bezier_expr =
  expression "gr_curve"
    (bezier_build <$> pts_expr <*> optional stroke_expr <*> optional fill_expr)
;;

let in_bom_expr = yesno_expr "in_bom"
let on_board_expr = yesno_expr "on_board"

(* property definition *)

let id_expr = int_expr "id"
;;

let horizontal_justify_atom =
  ws *> choice
    [
      string "left" *> return J_left
    ; string "right" *> return J_right
    ; string "center" *> return J_center
    ]

let vertical_justify_atom =
   ws *> choice
     [
       string "top" *> return J_top
     ; string "bottom" *> return J_bottom
     ; string "center" *> return J_center
     ]

type justification =
  {
    horiz: justify
  ; vert: justify
  }

let justify_expr =
  let content = (fun horiz vert -> {horiz; vert}) <$> header "justify" *> horizontal_justify_atom <*> vertical_justify_atom in
  parens content
;;

let italic_atom = keyword "italic" *> return true
let optional_italic_atom = option false italic_atom

let bold_atom = keyword "bold" *> return true
let optional_bold_atom = option false bold_atom

type font_def =
  {
    font: string option
  ; size: coord
  ; italic: bool
  ; bold: bool
  ; kolor: kolor option
  }

let size_expr = expression "size" coords

let font_expr =
  expression "font" ((fun font size italic bold kolor: font_def -> {font; size; italic; bold; kolor}) <$> optional escaped_string_atom <*> size_expr <*> optional_italic_atom <*> optional_bold_atom <*> optional color_expr)

let optional_font_expr = optional font_expr
;;

let optional_hide_atom = option false ((fun _ -> true) <$> keyword "hide" )

type effects =
  {
    font: font_def
  ; justify: justification option
  ; hide: bool
  }

let effects_expr =
  expression "effects" ((fun font justify hide -> {font; justify; hide}) <$> font_expr <*> optional justify_expr <*> optional_hide_atom)
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
(* (fun name _ value id at effects -> {name; value; id; at; effects}) *)
let property_expr =
  expression "property"
    ((fun name _ value id at effects -> {name; value; id; at; effects}) <$>
     escaped_string_atom <*> space <*> escaped_string_atom <*> id_expr <*> pin_at_coord_expr <*> optional effects_expr)
;;

let text_build text coords effects =
  let Coord (size, _) = effects.font.size in
  Text {c=make_rel coords; text; s=Size size }

let text_expr =
  expression "text" (text_build <$> escaped_string_atom <*> pin_at_coord_expr <*> effects_expr)

;;
(* Structures to describe drawing of  symbols *)

let pin_tag_expr s =
  expression s
    ((fun s e -> let Coord(x, _) = e.font.size in s, (Size x))
     <$> escaped_string_atom <*> effects_expr)

let length_expr =
  dist_expr "length"

let pin_type_atom =
  ws *> choice
    [
      string "bidirectional"
    ; string "passive"
    ; string "input"
    ; string "output"
    ; string "power_in"
    ; string "power_out"
    ; string "open_collector"
    ; string "open_drain"
    ; string "emitter_follower"
    ; string "source_follower"
    ; string "unconnected"
    ; string "tristate"
    ; string "unspecified"
    ]
let pin_shape_atom =
  ws *> choice
    [
      string "none"
    ; string "line"
    ; string "inverted"
    ; string "clock"
    ; string "inverted_clk"
    ; string "input_low"
    ; string "clock_low"
    ; string "falling_edge"
    ; string "non_logic"
    ]

let pin_expr =
  expression "pin" ((fun _ _ (c,a) s name number ->
      let contact = make_rel c in
      let orient = match a with
        | 0 -> P_R
        | 90 -> P_U
        | 180 -> P_L
        | 270 -> P_D
        | _ -> P_R (* Error *)
      in
      let length = Size s in
      Pin {name; number; length; contact; orient})
                    <$> pin_type_atom <*> pin_shape_atom
                    <*> pin_at_expr <*> length_expr
                    <*> pin_tag_expr "name" <*> pin_tag_expr "number")
;;

(* The symbols *)

let primitive_expr =
  choice
    [ polyline_expr
    ; circle_expr
    ; arc_expr
    ; bezier_expr
    ; pin_expr
    ; text_expr
    ]

let primitive_list_expr =
  many primitive_expr

let extend_expr = string_expr "extends"

let elts name _properties graphics pins =
  (* let prop = List.filter_map ~f:(Option.map (fun prim -> {parts=1; prim})) properties in *)
  match List.rev (String.split_on_char ~sep:'_' name) with
  | _style::part_str::_ -> let parts=int_of_string part_str in
    let g = List.map ~f:(fun prim -> {parts; prim}) graphics in
    let p = List.map ~f:(fun prim -> {parts; prim}) pins in
    List.concat  [g; p]
  | _ -> failwith "malformed unit name"

let unit_build name graphics pins = elts name [] graphics pins

let unit_expr =
  expression "symbol"
    ( unit_build <$> escaped_string_atom <*> (many primitive_expr) <*> (many pin_expr))
;;

let symbol_build name _extends hide_pin_numbers pin_names _in_bom _on_board _properties units =
  let draw_pname = match pin_names with Some _ -> false | None -> true in
  let graph =  List.flatten units in
  {names=[name]; draw_pnum= not hide_pin_numbers; draw_pname; multi=false; graph}

let symbol_expr =
  expression "symbol"
    ( symbol_build <$> escaped_string_atom <*> optional extend_expr <*> optional_pin_number_expr <*> optional_pin_names_expr <*> in_bom_expr <*> on_board_expr <*> (many1 property_expr) <*> (many1 unit_expr))

;;

let lib_symbols_expr =
  expression "lib_symbols" (fix (fun m -> (add_component <$> symbol_expr <*> m) <|> (return (lib ()))))
