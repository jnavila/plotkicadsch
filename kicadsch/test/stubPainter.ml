open Kicadsch.Sigs

type t = string list

let string_of_justification = function
  | J_left   -> "J_left"
  | J_right  -> "J_right"
  | J_center -> "J_center"
  | J_bottom -> "J_bottom"
  | J_top    -> "J_top"

let string_of_style = function
  | Bold -> "Bold"
  | Italic -> "Italic"
  | BoldItalic -> "BoldItalic"
  | NoStyle -> "NoStyle"

let string_of_orientation = function
  | Orient_H -> "Orient_H"
  | Orient_V -> "Orient_V"

let string_of_kolor = function
  | `NoColor -> "NoColor"
  | `Black -> "Black"
  | `Green -> "Green"
  | `Red -> "Red"
  | `Blue -> "Blue"
  | `Brown -> "Brown"

let paint_text ?(kolor=`Black) t (o:orientation) (Coord (x,y)) (Size size) justif styl c =
  (Printf.sprintf "Text %s %s %s %d %d %d %s %s" (string_of_kolor kolor) t (string_of_orientation o) x y size (string_of_justification justif) (string_of_style styl) ):: c

let paint_line ?(kolor=`NoColor) ?(width=Size 1) (Coord (x1, y1)) (Coord (x2, y2)) c =
  (Printf.sprintf "Line %d %d - %d %d" x1 y1 x2 y2) :: c

let paint_rect ?(kolor=`NoColor) ?(fill=`NoColor) (Coord(x, y)) (Coord (dim_x, dim_y)) c =
  c

let paint_circle ?(kolor=`NoColor) ?(fill=`NoColor) (Coord(x, y)) radius c =
  c

let paint_arc ?(kolor=`NoColor) ?(fill=`NoColor) c1 c2 c3 r c =
  c

let paint_image co s b c =
  c
let get_context () = []

let set_canevas_size _ _ c =
  c

let write c = c
