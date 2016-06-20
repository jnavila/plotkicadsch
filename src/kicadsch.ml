type orientation = Orient_H | Orient_V
type coord = Coord of (int*int)
type size = Size of int
type justify = J_left | J_right | J_center | J_bottom | J_top
type style = Bold | Italic | BoldItalic | NoStyle

    
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
      
type schContext =
    BodyContext
  | WireContext
  | ComponentContext
  | TextContext

let initial_context  = BodyContext

(* SVG stuff *)
  
open Svg
open Svg.M
open Svg_types.Unit

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

let svg_text t o (Coord (x,y)) size justif styl =
  let size_in = Printf.sprintf "%f"  (float_of_int size) and
      j = anchor_attr_of_justify justif and
      s = style_attr_of_style styl
  in
  text ~a:([a_x_list [float_of_int x,None] ; a_y_list [float_of_int y,None] ; a_fontsize size_in; j]@s) [pcdata t]

let svg_line (Coord (x1, y1)) (Coord (x2, y2)) =
  let x1_in = float_of_int x1 and
      y1_in = float_of_int y1 and
      x2_in = float_of_int x2 and
      y2_in = float_of_int y2 in 
  polyline ~a:([a_points [(x1_in, y1_in); (x2_in, y2_in) ]; a_strokewidth (1., Some `Px); a_stroke (`Color ("#000000",None)) ]) []
    
(* Parsing a sch file *)

let regex_F = Pcre.regexp "F [\\d-]+ \"([^\"]*)\" (H|V) ([\\d-]+) ([\\d-]+) ([\\d-]+)? +(0|1)(0|1)(0|1)(0|1) (L|R|C|B|T) (L|R|C|B|T)(I|N)(B|N)"(*"" *)
    
let parse_F line =
  try
    let sp = Pcre.extract ~rex:regex_F line in
    if (String.get sp.(9) 0 == '0') then
      let c = Coord (int_of_string sp.(3), int_of_string sp.(4)) and
          o = orientation_of_string sp.(2) in
      Some (svg_text sp.(1) o c (int_of_string sp.(5)) (justify_of_string sp.(10)) (style_of_string (sp.(12), sp.(13))))
    else
      None
    with | Not_found -> (print_endline (Printf.sprintf "could not match %s" line); None) 

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


let regex_wire = Pcre.regexp "\\t([\\d-]+) +([\\d-]+) +([\\d-]+) +([\\d-]+)"

let parse_wire_line line =
  try
    let sp = Pcre.extract ~rex:regex_wire line in
    let c1 = Coord (int_of_string sp.(1), int_of_string sp.(2)) and
        c2 = Coord (int_of_string sp.(3), int_of_string sp.(4)) 
    in
    Some (svg_line c1 c2)
  with | Not_found -> (print_endline (Printf.sprintf "could not match wire (%s)" line); None) 

let regex_noconn = Pcre.regexp "NoConn ~ ([\\d-]+) +([\\d-]+)"

let parse_noconn_line line =
  try
    let sp = Pcre.extract ~rex:regex_noconn line in
    let x = int_of_string sp.(1) and y=int_of_string sp.(2) in
    let delta = 20 in
    Some ( svg [ svg_line (Coord (x - delta, y - delta)) (Coord (x + delta, y + delta)) ; svg_line (Coord (x - delta, y + delta)) (Coord (x + delta, y - delta)) ])
  with
    Not_found -> (print_endline (Printf.sprintf "could not match noconn (%s)" line); None)

let parse_body_line c line =
  if (String.compare line "$Comp" == 0) then
    (ComponentContext, None)
  else if (String.compare (String.sub line 0 4) "Wire" == 0) then
    WireContext, None
  else if (String.compare (String.sub line 0 6) "NoConn" == 0) then
    BodyContext, parse_noconn_line line
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
  | TextContext -> (BodyContext, None)
