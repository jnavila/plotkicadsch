type orientation = Orient_H | Orient_V
type coord = Coord of (int*int)
type size = Size of int
type justify = J_left | J_right | J_center | J_bottom | J_top
type style = Bold | Italic | BoldItalic | NoStyle
type kolor = NoColor | Black | Green | Red


module type Painter = sig

  type t

  val paint_text: ?kolor: kolor -> String.t -> orientation -> coord -> size -> justify -> style -> t -> t

  val paint_line: coord -> coord -> t -> t

  val paint_circle: ?fill: kolor -> coord -> int -> t -> t

  val paint_rect: ?fill: kolor -> coord -> coord -> t -> t

  val get_context: unit -> t

  val write: out_channel -> t -> unit
end

module type SchPainter = sig
  type schContext

  val initial_context : Kicadlib.t -> schContext

(**

**)
  val parse_line :
    String.t -> schContext -> schContext


  val output_context: schContext -> out_channel -> unit

end
(* For test only
val parse_F : ?context:int -> String.t -> SvgPainter.t -> SvgPainter.t

val parse_wire_line : ?context:int -> String.t -> SvgPainter.t -> SvgPainter.t
*)
