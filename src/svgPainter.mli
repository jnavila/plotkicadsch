type orientation = Orient_H | Orient_V
type coord = Coord of (int*int)
type size = Size of int
type justify = J_left | J_right | J_center | J_bottom | J_top
type style = Bold | Italic | BoldItalic | NoStyle
type kolor = NoColor | Black | Green | Red

type t

val svg_text: ?kolor: kolor -> String.t -> orientation -> coord -> size -> justify -> style -> t -> t

val svg_line: coord -> coord -> t -> t

val svg_circle: ?fill: kolor -> coord -> int -> t -> t

val svg_rect: ?fill: kolor -> coord -> coord -> t -> t

val svg_get_context: unit -> t

val svg_write: out_channel -> t -> unit
