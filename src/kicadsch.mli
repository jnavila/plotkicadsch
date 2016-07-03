
type schContext

val initial_context : schContext

(**

**)
val parse_line :
  String.t -> schContext -> schContext


val output_context: schContext -> out_channel -> unit

(* For test only *)
val parse_F : ?context:int -> String.t -> SvgPainter.t -> SvgPainter.t

val parse_wire_line : ?context:int -> String.t -> SvgPainter.t -> SvgPainter.t
