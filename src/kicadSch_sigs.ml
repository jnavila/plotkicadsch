(**
   Kicad modules signatures *)

(** orientation of a text *)
type orientation = Orient_H | Orient_V

(** absolute coordinates in the drawing *)
type coord = Coord of int*int

(** font size of a text *)
type size = Size of int

(** Text justification of a text *)
type justify = J_left | J_right | J_center | J_bottom | J_top

(** Style of a text *)
type style = Bold | Italic | BoldItalic | NoStyle

(** Color of the text. These are the colors appearing ing Kicad schematics *)
type kolor = NoColor | Black | Green | Red | Blue | Brown

(** Transformation matrix of a relative coordinate around an absolute coordinate.
    The matrix is layed out as a pair of two lines of pairs *)
type transfo = ((int * int) * (int * int))

(** A module able to paint a canvas with several graphic primitives
   and then to process the canvas into a picture file format.  The
   functions are supposed to be pure *)
module type Painter = sig

  (** the canvas of the painter *)
  type t

  (** [paint ?kolor text orient coord size justification style canvas]
     adds a [text] with the given [orient], [size], [justification]
     and [style] at the given [coord] to [canvas].@return the modified canvas *)
  val paint_text: ?kolor: kolor -> String.t -> orientation -> coord -> size -> justify -> style -> t -> t

  (** [paint_line ?kolor width start end canvas] paints a line with
     the given [kolor] and [width] between [start] and [stop] on
     [canvas]. @return the modified canvas *)
  val paint_line: ?kolor: kolor -> ?width: size -> coord -> coord -> t -> t

  (** [paint_circle ?kolor center radius canvas] paints a circle
     filled with the given [kolor] defined by [center] and [radius] on
     [canvas]. @return the modified canvas *)
  val paint_circle: ?fill: kolor -> coord -> int -> t -> t

  (** [paint_rect ?kolor corner1 corner2 canvas] paints a rectangle
     filled with the given [kolor] defined by [corner1] and [corner2]
     on [canvas]. @return the modified canvas *)
  val paint_rect: ?fill: kolor -> coord -> coord -> t -> t

  (** [paint_image corner scale png canvas] paints a [png] image
     filled at [corner], scaled at [scale] on [canvas]. @return the
     modified canvas *)
  val paint_image: coord -> float -> Buffer.t -> t -> t

  (** [paint_arc ?kolor start end radius canvas] paints an arc filled
     with [kolor] between [start] and [end] of [radius] on
     [canvas]. @return the modified canvas *)
  val paint_arc: ?fill:kolor -> coord -> coord -> int -> t -> t

  (** [set_canevas x y canvas] set the size of the canevas
      @return the modified canvas *)
  val set_canevas_size: int -> int -> t -> t

  (** [get_context ()] @return a new canvas *)
  val get_context: unit -> t

end

(** A module able to paint a schematic in a schematic context *)
module type SchPainter = sig
  (** the schematic context *)
  type schContext

  (** the underlying context **)
  type painterContext

  (** [initial_context ()] @return an new empty context *)
  val initial_context : unit -> schContext

  (** [add_lib line context] parse the content of [line] provided to
     libs to the [context]. @return the updated context *)
  val add_lib: string -> schContext -> schContext

  (** [parse_line line context] parse a new [line] of schematic and
     update [context]. @return the updated context *)
  val parse_line :
    String.t -> schContext -> schContext

  (** [output_context context output] write the [context] as a image
     format to [output] *)
  val output_context: schContext -> painterContext

end

(** Library component painter *)
module type CompPainter =
sig

  (** A component Library manager *)
  type t

  (** A drawing context *)
  type drawContext

  (** [lib ()] @return an empty new component manager *)
  val lib: unit -> t

  (** [append_lib stream context] appends the lib contained in the
     [stream] to the context. @return the updated context *)
  val append_lib: string -> t -> t

  (** [plot_comp lib name partnumber origin transformation context]
     find in [lib] the component with given [name] and plot the part
     [partnumber] at [origin] after [transfomation] into the graphical
     [context]. @return the updated graphical context *)
  val plot_comp: t -> string -> int -> coord -> transfo -> drawContext -> drawContext

end
