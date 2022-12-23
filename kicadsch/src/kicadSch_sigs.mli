(**
   Kicad modules Signatures *)

open KicadDefs

(** orientation of a text *)
type orientation = Orient_H | Orient_V  (** *)

(** absolute coordinates in the drawing *)
type coord = Coord of int * int

(** Text justification of a text *)
type justify = J_left | J_right | J_center | J_bottom | J_top  (** *)

(** Style of a text *)
type style = Bold | Italic | BoldItalic | NoStyle  (** *)

(** Color of the text. These are the colors appearing in Kicad schematics *)
type kolor = [`NoColor | `Black | `Green | `Red | `Blue | `Brown]

(** Transformation matrix of a relative coordinate around an absolute coordinate.
    The matrix is layed out as a pair of lines of pairs *)
type transfo = (int * int) * (int * int)

type revision =
  | First of string
  | Second of string
  | No_Rev

type portrange = Glabel | Hlabel

type porttype =
  | UnSpcPort
  | ThreeStatePort
  | OutputPort
  | InputPort
  | NoPort
  | BiDiPort

type labeluse = WireLabel | TextNote

type labeltype = PortLabel of portrange * porttype | TextLabel of labeluse

type label = {c: coord; size: size; orient: justify; labeltype: labeltype}

type field =
  { nb: int
  ; text: string
  ; o: orientation
  ; co: coord
  ; s: size
  ; j: justify
  ; stl: style }

type single_reference = {piece: string option; unitnr: int option}

type multi_reference = {m_piece: string; m_unitnr: int}

type bitmapContext =
  {pos: coord option; scale: float option; data: Buffer.t option}

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

  val initial_context : ?allow_missing_component:bool -> revision -> schContext
  (** [initial_context allow_missing_component revision]
      @return an new empty context *)

  val add_lib : string -> schContext -> schContext
  (** [add_lib content context] parse the [content] provided to
      libs to the [context].
      @return the updated context *)

  val parse_sheet : schContext -> String.t -> schContext
  (** [parse_line content context] parse a [content] of schematic and
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
  type t = KicadLib_sigs.library

  (** A drawing context *)
  type drawContext

  val plot_comp :
    t -> string -> int -> coord -> transfo -> bool -> drawContext -> drawContext * bool
    (** [plot_comp lib name partnumber origin transformation
       allow_missing context] find in [lib] the component with given
       [name] and plot the part [partnumber] at [origin] after
       [transformation] into the graphical [context] and the fact that
       the component is multipart. If the component is not found, raise
       an exception, unless [allow_missing] is true.

       @return the updated graphical context *)
end
