type orientation = Orient_H | Orient_V
type coord = Coord of (int*int)
type size = Size of int
type justify = J_left | J_right | J_center | J_bottom | J_top
type style = Bold | Italic | BoldItalic | NoStyle
type kolor = NoColor | Black | Green | Red | Blue | Brown
type transfo = ((int * int) * (int * int))

module type Painter = sig

  type t

  val paint_text: ?kolor: kolor -> String.t -> orientation -> coord -> size -> justify -> style -> t -> t

  val paint_line: ?kolor: kolor -> ?width: size -> coord -> coord -> t -> t

  val paint_circle: ?fill: kolor -> coord -> int -> t -> t

  val paint_rect: ?fill: kolor -> coord -> coord -> t -> t

  val paint_arc: ?fill:kolor -> coord -> coord -> int -> t -> t

  val get_context: unit -> t

  val write: Lwt_io.output_channel -> t -> unit Lwt.t
end

module type SchPainter = sig
  type schContext

  val initial_context : unit -> schContext

  val add_lib: string Lwt_stream.t -> schContext -> schContext Lwt.t

  val parse_line :
    String.t -> schContext -> schContext

  val output_context: schContext -> Lwt_io.output_channel -> unit Lwt.t

end


module type CompPainter =
sig
  type t
  type drawContext
  val lib: unit -> t
  val append_lib: string Lwt_stream.t -> t -> t Lwt.t
  val plot_comp: t -> string -> int -> coord -> transfo -> drawContext -> drawContext

end
