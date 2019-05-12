(**
   Kicad modules Signatures *)

(** orientation of a text *)
type orientation = Orient_H | Orient_V  (** *)

(** absolute coordinates in the drawing *)
type coord = Coord of int * int

(** font size of a text *)
type size = Size of int  (** *)

(** Text justification of a text *)
type justify = J_left | J_right | J_center | J_bottom | J_top  (** *)

(** Style of a text *)
type style = Bold | Italic | BoldItalic | NoStyle  (** *)

(** Color of the text. These are the colors appearing in Kicad schematics *)
type kolor = [`NoColor | `Black | `Green | `Red | `Blue | `Brown]

(** Transformation matrix of a relative coordinate around an absolute coordinate.
    The matrix is layed out as a pair of lines of pairs *)
type transfo = (int * int) * (int * int)

module type Painter = sig
  (** A module able to paint a canvas with several graphic primitives
      and then to process the canvas into a picture file format.  The
      functions are supposed to be pure *)

  (** the canvas of the painter *)
  type t

  val paint_text :
       ?kolor:kolor
    -> String.t
    -> orientation
    -> coord
    -> size
    -> justify
    -> style
    -> t
    -> t
  (** [paint ?kolor text orient coord size justification style canvas]
      adds a [text] with the given [orient], [size], [justification]
      and [style] at the given [coord] to [canvas].
      @return the modified canvas *)

  val paint_line : ?kolor:kolor -> ?width:size -> coord -> coord -> t -> t
  (** [paint_line ?kolor width start end canvas] paints a line with
      the given [kolor] and [width] between [start] and [stop] on
      [canvas].
      @return the modified canvas *)

  val paint_circle : ?kolor:kolor -> ?fill:kolor -> coord -> int -> t -> t
  (** [paint_circle ?kolor center radius canvas] paints a circle
      filled with the given [kolor] defined by [center] and [radius] on
      [canvas].
      @return the modified canvas *)

  val paint_rect : ?kolor:kolor -> ?fill:kolor -> coord -> coord -> t -> t
  (** [paint_rect ?kolor corner1 corner2 canvas] paints a rectangle
      filled with the given [kolor] defined by [corner1] and [corner2]
      on [canvas].
      @return the modified canvas *)

  val paint_image : coord -> float -> Buffer.t -> t -> t
  (** [paint_image corner scale png canvas] paints a [png] image
      filled at [corner], scaled at [scale] on [canvas].
      @return the
      modified canvas *)

  val paint_arc :
    ?kolor:kolor -> ?fill:kolor -> coord -> coord -> coord -> int -> t -> t
  (** [paint_arc ?kolor center start end radius canvas] paints an arc filled
      with [kolor] between [start] and [end] of [radius] around center on
      [canvas].
      @return the modified canvas *)

  val set_canevas_size : int -> int -> t -> t
  (** [set_canevas x y canvas] set the size of the canevas

      @return the modified canvas *)

  val get_context : unit -> t
  (** [get_context ()]
      @return a new painting canvas *)
end

module type SchPainter = sig
  (** A module able to paint a schematic file in a painter context *)

  (** the schematic context *)
  type schContext

  (** the underlying context *)
  type painterContext

  val initial_context : unit -> schContext
  (** [initial_context ()]
      @return an new empty context *)

  val add_lib : string -> schContext -> schContext
  (** [add_lib line context] parse the content of [line] provided to
      libs to the [context].
      @return the updated context *)

  val parse_line : String.t -> schContext -> schContext
  (** [parse_line line context] parse a new [line] of schematic and
      update [context].
      @return the updated context *)

  val output_context : schContext -> painterContext
  (** [output_context context output] write the [context] as a image
      format to [output] *)
end

module type CompPainter = sig
  (** The library that is able to read component libraries and
      memorize the read components. Then when passed a drawing context
      and a component to paint it can paint the component on demand to
      the drawing context *)

  (** A component Library manager *)
  type t

  (** A drawing context *)
  type drawContext

  val lib : unit -> t
  (** [lib ()]
      @return an empty new component manager *)

  val append_lib : string -> t -> t
  (** [append_lib stream context] appends the lib contained in the
      [stream] to the context.
      @return the updated context *)

  val plot_comp :
    t -> string -> int -> coord -> transfo -> drawContext -> drawContext * bool
  (** [plot_comp lib name partnumber origin transformation context]
      find in [lib] the component with given [name] and plot the part
      [partnumber] at [origin] after [transfomation] into the graphical
      [context] and the fact that the component is multipart.
      @return the updated graphical context *)
end
