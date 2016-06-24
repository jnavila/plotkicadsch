type orientation = Orient_H | Orient_V
type coord = Coord of (int*int)
type size = Size of int
type justify = J_left | J_right | J_center | J_bottom | J_top
type style = Bold | Italic | BoldItalic | NoStyle
type kolor = NoColor | Black
    
let justify_of_string s =
  match String.get s 0 with
  | 'L' -> J_left
  | 'R' -> J_right
  | 'C' -> J_center
  | 'B' -> J_bottom
  | 'T' -> J_top
  | c -> failwith (Printf.sprintf "no match for justify! (%c)" c)
     
let style_of_string s =
  let i = String.get (fst s) 0 and
      b = String.get (snd s) 0 in
  match i,b with
  | 'N','B' -> Bold
  | 'I', 'N' -> Italic
  | 'I', 'B' -> BoldItalic
  | 'N', 'N' -> NoStyle
  | _ -> failwith   (Printf.sprintf "no match for style! (%c %c)" i b)

let orientation_of_string s =
  match String.get s 0 with
  | 'H' -> Orient_H
  | 'V' -> Orient_V    
  | c -> failwith (Printf.sprintf "no match for orientation! (%c)" c)

type rect = { c:coord ; dim:coord }

type schContext =
    BodyContext
  | WireContext
  | ComponentContext
  | SheetContext of rect option
  | TextContext of (coord*int*orientation) option

let initial_context  = BodyContext

(* SVG stuff *)

open Svg
open Svg.M
open Svg_types.Unit
open Svg_types


let style_attr_of_style = function
  | Italic -> [a_fontstyle "italic"]
  | Bold -> [a_fontweight "bold"]
  | BoldItalic -> [a_fontstyle "italic"; a_fontweight "bold"]
  | NoStyle -> []

let anchor_attr_of_justify justif = 
      a_text_anchor (match justif with
      | J_left -> `Start
      | J_center -> `Middle
      | J_right -> `End
      | J_bottom -> `End
      | J_top -> `Start)

let color_of_kolor = function
  | NoColor -> `Color ("none",None)
  | Black -> `Color ("#000000",None)

(** SVG coord type conversion from int **)
let coord_of_int x = float_of_int x, None

let svg_text t o (Coord (x,y)) size justif styl =
  let size_in = Printf.sprintf "%f"  (float_of_int size) and
      j = anchor_attr_of_justify justif and
      s = style_attr_of_style styl
  in
  text ~a:([a_x_list [coord_of_int x] ; a_y_list [coord_of_int y] ; a_fontsize size_in; j]@s) [pcdata t]

let svg_line (Coord (x1, y1)) (Coord (x2, y2)) =
  let x1_in = float_of_int x1 and
      y1_in = float_of_int y1 and
      x2_in = float_of_int x2 and
      y2_in = float_of_int y2 in 
  polyline ~a:([a_points [(x1_in, y1_in); (x2_in, y2_in) ]; a_strokewidth (1., Some `Px); a_stroke ( color_of_kolor Black) ]) []

let svg_rect ?(fill=NoColor) (Coord(x, y)) (Coord (dim_x, dim_y)) =
  rect ~a:[ a_x (coord_of_int x); a_y (coord_of_int y); a_width (coord_of_int dim_x); a_height (coord_of_int dim_y);a_fill (color_of_kolor fill); a_strokewidth (1., Some `Px); a_stroke (color_of_kolor Black)] []

let svg_circle ?(fill=NoColor) (Coord(x, y)) radius =
  circle ~a:[a_r (coord_of_int radius); a_cx (coord_of_int x); a_cy (coord_of_int y); a_fill (color_of_kolor fill); a_strokewidth (1., Some `Px); a_stroke (color_of_kolor Black) ] []

(* Parsing a sch file *)

let create_sch_parse_fun ~name ~regexp_str ~processing =
  let regexp = Pcre.regexp regexp_str in
  let parser ?context line =
    try
      let sp = Pcre.extract ~rex:regexp line in
      processing context sp
    with Not_found ->
      (print_endline (Printf.sprintf "could not match %S (%s)" name line); None)
  in parser

let parse_F = create_sch_parse_fun
  ~name:"Component F"
  ~regexp_str:"F [\\d-]+ \"([^\"]*)\" (H|V) ([\\d-]+) ([\\d-]+) ([\\d-]+)? +(0|1)(0|1)(0|1)(0|1) (L|R|C|B|T) (L|R|C|B|T)(I|N)(B|N)"
  ~processing:
  (fun context sp ->
    if (String.get sp.(9) 0 == '0') then
      let c = Coord (int_of_string sp.(3), int_of_string sp.(4)) and
          o = orientation_of_string sp.(2) in
      Some (svg_text sp.(1) o c (int_of_string sp.(5)) (justify_of_string sp.(10)) (style_of_string (sp.(12), sp.(13))))
    else
      None
  )

let parse_component_line line =
  let first = String.get line 0 in
  match first with
           (* | 'L' ->
              let sp = parse "L (\\w+) (\\w+)" in
              -> None
              | 'U' ->
              let sp = parse "U ([\\d-]+) ([\\d-]+) (\\w+)" in
              parse_fields ic {comp with a= int_of_string sp.(0)} *)
  | 'F' -> parse_F line
  | _ -> None

let parse_wire_line = create_sch_parse_fun
  ~name:"Wire"
  ~regexp_str:"\\t([\\d-]+) +([\\d-]+) +([\\d-]+) +([\\d-]+)"
  ~processing:
  (fun context sp ->
    let c1 = Coord (int_of_string sp.(1), int_of_string sp.(2)) and
        c2 = Coord (int_of_string sp.(3), int_of_string sp.(4))
    in
    Some (svg_line c1 c2)
  )

let parse_noconn_line = create_sch_parse_fun
  ~name:"NoConn"
  ~regexp_str:"NoConn ~ ([\\d-]+) +([\\d-]+)"
  ~processing:
  (fun context sp ->
    let x = int_of_string sp.(1) and y=int_of_string sp.(2) in
    let delta = 20 in
    Some ( svg [ svg_line (Coord (x - delta, y - delta)) (Coord (x + delta, y + delta)) ; svg_line (Coord (x - delta, y + delta)) (Coord (x + delta, y - delta)) ])
  )

let parse_conn_line = create_sch_parse_fun
  ~name:"Connection"
  ~regexp_str:"Connection ~ ([\\d-]+) +([\\d-]+)"
  ~processing:(fun context sp  ->
    let x = int_of_string sp.(1) and y=int_of_string sp.(2) in
    let delta = 10 in
    Some (svg_circle ~fill:Black (Coord (x,y)) delta)
  )

let parse_sheet_field = create_sch_parse_fun
  ~name:"Sheet Field"
  ~regexp_str:"F(0|1) +\"([^\"]*)\" +([\\d-]+)"
  ~processing:(fun context sp ->
    let number= int_of_string sp.(1) and
        name = sp.(2) and
        size = int_of_string sp.(3) in
    (match context with
    | Some(Some {c=Coord (x, y); dim=Coord (dim_x, dim_y)}) ->
       let y = if (number == 0) then y else y + dim_y + size in
       Some (svg_text name Orient_H (Coord (x, y))  size J_left NoStyle)
    | None | Some(None)-> None)
  )

let parse_sheet_rect = create_sch_parse_fun
  ~name:"Sheet Rect"
  ~regexp_str:"S +([\\d-]+) +([\\d-]+) +([\\d-]+) +([\\d-]+)"
  ~processing:(fun context sp ->
       let c = Coord (int_of_string sp.(1), int_of_string sp.(2)) and
        dim = Coord (int_of_string sp.(3), int_of_string sp.(4))
       in
       Some {c;dim}
  )

let parse_text_line = create_sch_parse_fun
  ~name:"Text header"
  ~regexp_str: "Text (H|G)?Label ([\\d-]+) ([\\d-]+) ([\\d-]+)    ([\\d-]+)   ~"
  ~processing: (fun context sp ->
    let c = Coord (int_of_string sp.(2), int_of_string sp.(3)) and
        orient = orientation_of_int(int_of_string sp.(4)) and
        dim = int_of_string sp.(5) in
    Some (c, dim, orient)
  )

let print_text_line line (c,dim,orient) =
  Some(svg_text line orient c dim J_left NoStyle)

let parse_sheet_line line context =
  match (String.get line 0) with
  | 'F' -> (
    match parse_sheet_field ~context line with
     | None -> None, None
     | Some _ as l -> context, l
  )
  | 'S' -> (
    match parse_sheet_rect ~context line with
    | None -> None, None
    |Some{c;dim} -> Some {c;dim}, Some (svg_rect c dim)
  )
  | 'U' ->
     context, None
  | _ -> (print_endline (Printf.sprintf "unknown sheet line (%s)" line); context, None)

let parse_body_line c line =
  if (String.compare line "$Comp" == 0) then
    ComponentContext, None
  else if (String.compare (String.sub line 0 4) "Wire" == 0) then
    WireContext, None
  else if (String.compare (String.sub line 0 6) "NoConn" == 0) then
    BodyContext, parse_noconn_line line
  else if (String.length line > 10) && (String.compare (String.sub line 0 10) "Connection" == 0) then
    BodyContext, parse_conn_line line
  else if (String.compare line "$Sheet" == 0) then
    SheetContext None, None
  else if (String.length line > 5) && (String.compare (String.sub line 0 4) "Text" == 0) then
    TextContext (parse_text_line line), None
  else
    BodyContext, None

let parse_line c line =
  match c with
  | ComponentContext ->
     if (String.compare line "$EndComp" == 0) then
       (BodyContext, None)
     else
         (ComponentContext, parse_component_line line)
  | BodyContext ->
     parse_body_line c line
  | WireContext ->
     BodyContext, (parse_wire_line line)
  | SheetContext sc ->
     if (String.compare line "$EndSheet" == 0) then
       (BodyContext, None)
     else
       let nsc, o = parse_sheet_line line sc in
       SheetContext nsc, o
  | TextContext sc ->
     match sc with
     |None -> failwith "TextContext without definition!"
     |Some v -> (BodyContext, print_text_line line v)
