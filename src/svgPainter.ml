
open Tyxml_svg
open Svg_types
open KicadSch_sigs

type content = [ `Polyline | `Text | `Svg | `Rect | `Circle |`Path | `Image ]
type t =  content elt list

let style_attr_of_style = function
  | Italic -> [a_font_style "italic"]
  | Bold -> [a_font_weight "bold"]
  | BoldItalic -> [a_font_style "italic"; a_font_weight "bold"]
  | NoStyle -> []

let anchor_attr_of_justify justif =
      a_text_anchor (match justif with
      | J_left -> `Start
      | J_center -> `Middle
      | J_right -> `End
      | J_bottom -> `End
      | J_top -> `Start)

let color_of_kolor k =
  let cstring = match k with
  | NoColor -> "none"
  | Black -> "#000000"
  | Red -> "#FF0000"
  | Green -> "#00FF00"
  | Blue -> "#0000CD"
  | Brown -> "#800000"
  in `Color (cstring, None)

(** SVG coord type conversion from int **)
let coord_of_int x = float_of_int x, None

let paint_text ?(kolor=Black) t (o:orientation) (Coord (x,y)) (Size size) justif styl c =
  let size_in = Printf.sprintf "%f"  (float_of_int size) and
      j = anchor_attr_of_justify justif and
      s = style_attr_of_style styl and
      x_c = float_of_int x and
      y_c = float_of_int y and
      angle = match o with
      | Orient_H -> 0.
      | Orient_V -> (-90.) in
  let orient = (angle,None), Some(x_c,y_c)
  in
  let color = color_of_kolor kolor in
  (text ~a:([a_x_list [coord_of_int x] ; a_y_list [coord_of_int y] ; a_font_size size_in; j; a_transform[`Rotate orient]; a_fill color]@s) [pcdata t]) :: c

let paint_line ?(kolor=Black) ?(width=(Size 2)) (Coord (x1, y1)) (Coord (x2, y2)) c =
  let x1_in = float_of_int x1 in
  let y1_in = float_of_int y1 in
  let x2_in = float_of_int x2 in
  let y2_in = float_of_int y2 in
  let Size width = width in
  let fwidth = float_of_int width in
  (polyline ~a:([a_points [(x1_in, y1_in); (x2_in, y2_in) ]; a_stroke_width (fwidth, Some `Px); a_stroke ( color_of_kolor kolor) ]) []) :: c

let paint_rect ?(fill=NoColor) (Coord(x, y)) (Coord (dim_x, dim_y)) c =
  (rect ~a:[ a_x (coord_of_int x); a_y (coord_of_int y); a_width (coord_of_int dim_x); a_height (coord_of_int dim_y);a_fill (color_of_kolor fill); a_stroke_width (1., Some `Px); a_stroke (color_of_kolor Black)] []) :: c

let paint_circle ?(fill=NoColor) (Coord(x, y)) radius c =
  (circle ~a:[a_r (coord_of_int radius); a_cx (coord_of_int x); a_cy (coord_of_int y); a_fill (color_of_kolor fill); a_stroke_width (1., Some `Px); a_stroke (color_of_kolor Black) ] []) :: c

let paint_arc ?(fill=NoColor) (Coord(x1, y1)) (Coord (x2, y2)) radius c =
  (* not sure how this thing behaves. This setup seems to work *)
  let sweepflag = if y1>=y2 then 0 else 1 in
  ( path ~a:[a_d (Printf.sprintf "M%d,%d A%d,%d 0 0,%d %d,%d" x1 y1 radius radius sweepflag x2 y2); a_fill (color_of_kolor fill); a_stroke_width (1., Some`Px); a_stroke (color_of_kolor Black)] []
  ) :: c

let paint_image (Coord (x,y)) scale b c =
  Printf.printf "painting image! %f %d %d" scale x y;
  (image ~a:[a_x (coord_of_int x); a_y (coord_of_int y); a_height ((100. *. scale), Some `Percent); a_width ((100. *. scale), Some `Percent); a_xlink_href @@ "data:image/png;base64," ^ (B64.encode (Buffer.contents b)) ] [])
  :: c

let get_context () = []

let write oc c =
  let svg_doc = svg  ~a:[a_width (29.7, Some `Cm); a_height (21., Some `Cm); a_viewBox (0.,0., 11693., 8268.)] c in
  let s = Format.asprintf "%a" (Tyxml.Svg.pp ()) svg_doc in
(*  let fmt = Format.formatter_of_out_channel oc in
  Tyxml.Svg.pp () fmt svg_doc *)
  Lwt_io.write oc s
