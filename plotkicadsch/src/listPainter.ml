open Kicadsch.Sigs

type image_data = Buffer.t
(*
let hash_fold_style hsv (arg:style) =
  match arg with
  | Bold -> Hash.fold_int hsv 0
  | Italic -> Hash.fold_int hsv 1
  | BoldItalic -> Hash.fold_int hsv 2
  | NoStyle -> Hash.fold_int hsv 3

let hash_fold_justify hsv (arg:justify) =
  match arg with
  | J_left -> Hash.fold_int hsv 0
  | J_right -> Hash.fold_int hsv 1
  | J_center -> Hash.fold_int hsv 2
  | J_bottom -> Hash.fold_int hsv 3
  | J_top -> Hash.fold_int hsv 4

let hash_fold_size hsv (Size arg: size) =
  Hash.fold_int hsv arg

let hash_fold_coord hsv (Coord (x,y)) =
  Hash.fold_int (Hash.fold_int hsv x) y

let hash_fold_orientation hsv (arg: orientation) =
  match arg with
  | Orient_H -> Hash.fold_int hsv 0
  | Orient_V -> Hash.fold_int hsv 1

let hash_fold_kolor hsv = function
  | NoColor -> Hash.fold_int hsv 0
  | Black -> Hash.fold_int hsv 1
  | Green -> Hash.fold_int hsv 2
  | Red -> Hash.fold_int hsv 3
  | Blue -> Hash.fold_int hsv 4
  | Brown -> Hash.fold_int hsv 5

let hash_fold_image_data hsv _ = hsv

let g_compare a b = if a=b then 0 else 1

let compare_kolor = g_compare
let compare_style = g_compare
let compare_justify = g_compare
let compare_orientation = g_compare
let compare_size = g_compare
let compare_coord= g_compare
let compare_string = String.compare
let compare_int = g_compare
let compare_float = g_compare
let compare_image_data _ _ = 0

let sexp_of_t _ = Sexplib0.Sexp.Atom "test"
                  *)
type t =
  | Text of kolor*string*orientation*coord*size*justify*style
  | Line of kolor*size*coord*coord
  | Rect of kolor*coord*coord
  | Circle of kolor*coord*int
  | Arc of kolor*coord*coord*int
  | Image of coord*float*image_data
  | Format of coord
(*                           [@@deriving hash, compare]

let t_of_sexp _ = Line (Black, Size 0, Coord (0, 0), Coord (0,0))
 *)

type listcanevas = t list


module ListPainter = Kicadsch.MakeSchPainter( struct

    type t = listcanevas

    let paint_text ?(kolor=Black) text (o:orientation) coords s j stl ctx =
      Text (kolor, text, o, coords, s, j, stl) :: ctx
    let paint_line ?(kolor=Black) ?(width=(Size 2)) pt_start pt_end ctx =
      Line (kolor, width, pt_start, pt_end) :: ctx
    let paint_rect ?(fill=NoColor) pt dims ctx =
      Rect (fill, pt, dims) :: ctx
    let paint_circle ?(fill=NoColor) center radius ctx =
      Circle (fill, center, radius):: ctx
    let paint_arc ?(fill=NoColor) pt_start pt_stop radius  ctx =
      Arc (fill, pt_start, pt_stop, radius) :: ctx
    let paint_image corner scale b c =
      Image (corner, scale, b) :: c
    let get_context () = []
    let set_canevas_size x y c = (Format (Coord (x, y))) :: c

  end)
