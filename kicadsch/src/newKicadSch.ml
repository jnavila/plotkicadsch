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

let yesno_expr s =
  expression s  (choice
                   [
                     string "yes" *> return true
                   ; string "no"   *> return false
                   ])

let dist_expr s =
  expression s  (floating_point >>| (fun s -> wx_size s))
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

let width_expr = int_expr "width" >>| (fun w -> Width w)
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

let optional_stroke_expr =
  optional stroke_expr
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

let optional_hide_atom = option false (string "hide" *> return true)

let pin_names_expr =
  expression "pin_names" ((fun _o h  -> h )<$>  optional_offset_expr <*> optional_hide_atom)

let optional_pin_names_expr =
  option false pin_names_expr
;;

let in_bom_expr = yesno_expr "in_bom"
let on_board_expr = yesno_expr "on_board"

(* property definition *)

let id_expr = int_expr "id"
let at_expr = expression "at" ((fun c _ a -> c,a) <$> coords <*> space <*> option 0 integer_atom)
let at_coord_expr = at_expr >>| (fun (c, _a) -> c)
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
  expression "font" ((fun font size italic bold kolor -> {font; size; italic; bold; kolor}) <$> optional escaped_string_atom <*> size_expr <*> optional_italic_atom <*> optional_bold_atom <*> optional color_expr)

let optional_font_expr = optional font_expr
;;

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

let property_expr =
  expression "property"
    ((fun name _ value id at effects -> {name; value; id; at; effects}) <$>
     escaped_string_atom <*> space <*> escaped_string_atom <*> id_expr <*> at_coord_expr <*> optional effects_expr)

;;
(* Structures to describe drawing of  symbols *)

type polyline =
  {
    points: relcoord list
  ; stroke : stroke option
  ; fill : fill option
  }

let polyline_expr =
  expression "polyline"
    ((fun pts stroke fill -> let points = make_rel_pts pts in {points; stroke; fill})
     <$> pts_expr <*> optional stroke_expr <*> optional fill_expr)

;;

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
      {name; number; length; contact; orient})
                    <$> pin_type_atom <*> pin_shape_atom
                    <*> at_expr <*> length_expr
                    <*> pin_tag_expr "name" <*> pin_tag_expr "number")
;;

let start_point_expr =
  expression "start" coords >>| make_rel

let end_point_expr =
  expression "end" coords >>| make_rel

let optional_stroke_to_width s =
  match s with
    | None
    | Some {width=None; _} -> 1
    | Some {width=Some (Width w); _} -> w


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

let radius_expr =
  expression "radius" ((fun at length angles -> at, length, angles) <$>
                       at_coord_expr <*> length_expr <*> angles_expr)

let arc_build  sp  _mid_point ep radius_group stroke _ =
  let width = optional_stroke_to_width stroke in
  let s = Size width in
  let center, radius =
  match radius_group with
    | None -> RelCoord (0, 0), 0 (* TODO: use mid_point to compute center and radius: https://github.com/KiCad/kicad-source-mirror/blob/f353fc448b3c8e86818b52f66020f31d671ae048/libs/kimath/src/trigo.cpp#L386 *)
    | Some (at, length, _angles) -> make_rel at, length
  in Arc { sp; ep; s; radius; center}

let arc_expr =
  expression "arc"
    (arc_build <$> start_point_expr <*> optional mid_point_expr <*> end_point_expr <*> optional radius_expr <*> optional stroke_expr <*> optional fill_expr)
;;

let bezier_build _a _b _c = ()

let bezier_expr =
  expression "bezier"
    (bezier_build <$> pts_expr <*> optional stroke_expr <*> optional fill_expr)
