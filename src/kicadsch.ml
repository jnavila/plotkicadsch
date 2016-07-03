

open SvgPainter

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

let orientation_of_int = function
  | 0 -> Orient_H
  | 1 -> Orient_V
  | _ as d -> failwith (Printf.sprintf "no int value for orientation! (%d)" d)

type rect = { c:coord ; dim:coord }

type portrange = Glabel | Hlabel
type labeluse = WireLabel | TextNote
type porttype = UnSpcPort | ThreeStatePort | OutputPort | InputPort | NoPort

let porttype_of_string = function
  | "UnSpc" -> UnSpcPort
  | "3State" -> ThreeStatePort
  | "Output" -> OutputPort
  | "Input" -> InputPort
  | "~" -> NoPort
  |   _ as s -> Printf.printf "unknown port type %s\n" s; NoPort

type labeltype =
  | PortLabel of portrange * porttype
  | TextLabel of labeluse

type label = { c: coord; size: size; orient: orientation; labeltype: labeltype }

type schParseContext =
    BodyContext
  | WireContext
  | ComponentContext
  | SheetContext of rect option
  | TextContext of label option

type schContext = schParseContext * SvgPainter.t
let initial_context  = BodyContext, SvgPainter.svg_get_context ()

(* SVG stuff *)
(* Parsing a sch file *)

(** This function generates a parsing function which outputs an 'a option
    Note that some lines may not yield any correct output, so the output is an option. **)
let create_sch_parse_fun ~name ~regexp_str ~processing =
  let regexp = Pcre.regexp regexp_str in
  let parser ?context line c =
    try
      let sp = Pcre.extract ~rex:regexp line in
      processing context sp c
    with Not_found ->
      (Printf.printf "could not match %s (%s)\n" name line; c)
  in parser

let parse_F = create_sch_parse_fun
  ~name:"Component F"
  ~regexp_str:"F [\\d-]+ \"([^\"]*)\" (H|V) ([\\d-]+) ([\\d-]+) ([\\d-]+)? +(0|1)(0|1)(0|1)(0|1) (L|R|C|B|T) (L|R|C|B|T)(I|N)(B|N)"
  ~processing:
  (fun context sp c ->
    if (String.get sp.(9) 0 == '0') then
      let co = Coord (int_of_string sp.(3), int_of_string sp.(4)) and
          o = orientation_of_string sp.(2) in
      svg_text sp.(1) o co (Size (int_of_string sp.(5))) (justify_of_string sp.(10)) (style_of_string (sp.(12), sp.(13))) c
    else
      c
  )

let parse_component_line line c =
  let first = String.get line 0 in
  match first with
           (* | 'L' ->
              let sp = parse "L (\\w+) (\\w+)" in
              -> None
              | 'U' ->
              let sp = parse "U ([\\d-]+) ([\\d-]+) (\\w+)" in
              parse_fields ic {comp with a= int_of_string sp.(0)} *)
  | 'F' -> parse_F line c
  | _ -> c

let parse_wire_line = create_sch_parse_fun
  ~name:"Wire"
  ~regexp_str:"\\t([\\d-]+) +([\\d-]+) +([\\d-]+) +([\\d-]+)"
  ~processing:
  (fun context sp c->
    let c1 = Coord (int_of_string sp.(1), int_of_string sp.(2)) and
        c2 = Coord (int_of_string sp.(3), int_of_string sp.(4))
    in
    svg_line c1 c2 c
  )

let parse_noconn_line = create_sch_parse_fun
  ~name:"NoConn"
  ~regexp_str:"NoConn ~ ([\\d-]+) +([\\d-]+)"
  ~processing:
  (fun context sp c ->
    let x = int_of_string sp.(1) and y=int_of_string sp.(2) in
    let delta = 20 in
    c |> svg_line (Coord (x - delta, y - delta)) (Coord (x + delta, y + delta))
      |> svg_line (Coord (x - delta, y + delta)) (Coord (x + delta, y - delta))
  )

let parse_conn_line = create_sch_parse_fun
  ~name:"Connection"
  ~regexp_str:"Connection ~ ([\\d-]+) +([\\d-]+)"
  ~processing:(fun context sp c ->
    let x = int_of_string sp.(1) and y=int_of_string sp.(2) in
    let delta = 10 in
    svg_circle ~fill:Black (Coord (x,y)) delta c
  )

let parse_sheet_field = create_sch_parse_fun
  ~name:"Sheet Field"
  ~regexp_str:"F(0|1) +\"([^\"]*)\" +([\\d-]+)"
  ~processing:(fun context sp c ->
    let number= int_of_string sp.(1) and
        name = sp.(2) and
        size = int_of_string sp.(3) in
    (match context with
    | Some(Some {c=Coord (x, y); dim=Coord (dim_x, dim_y)}) ->
       let y = if (number == 0) then y else y + dim_y + size in
       svg_text name Orient_H (Coord (x, y))  (Size size) J_left NoStyle c
    | None | Some(None)-> c)
  )

let parse_sheet_rect = create_sch_parse_fun
  ~name:"Sheet Rect"
  ~regexp_str:"S +([\\d-]+) +([\\d-]+) +([\\d-]+) +([\\d-]+)"
  ~processing:(fun context sp (prev,canevas) ->
       let c = Coord (int_of_string sp.(1), int_of_string sp.(2)) and
        dim = Coord (int_of_string sp.(3), int_of_string sp.(4))
       in
       Some {c;dim}, (svg_rect c dim canevas)
  )

let parse_text_line = create_sch_parse_fun
  ~name:"Text header"
  ~regexp_str: "Text (GLabel|HLabel|Label|Notes) ([\\d-]+) ([\\d-]+) ([\\d-]+)    ([\\d-]+)   (~|UnSpc|3State|Output|Input)"
  ~processing: (fun context sp (dummy, canevas) ->
    let c = Coord (int_of_string sp.(2), int_of_string sp.(3)) and
        orient = orientation_of_int(int_of_string sp.(4)) and
        size = Size (int_of_string sp.(5)) in
    let labeltype =
      match sp.(1) with (* TODO: draw the connectors *)
    | "GLabel" -> PortLabel (Glabel, porttype_of_string sp.(6))
    | "HLabel" -> PortLabel (Hlabel, porttype_of_string sp.(6))
    | "Label" -> TextLabel WireLabel
    | "Notes" -> TextLabel TextNote
    | _ -> TextLabel TextNote in
    TextContext (Some {c; size; orient; labeltype}), canevas
  )

(* Printing things *)

let print_text_line line l c =
  match l.labeltype with
  | TextLabel t -> begin
    let pcolor = match t with
      | TextNote ->  Green
      | WireLabel -> Red in
    svg_text ~kolor:pcolor line l.orient l.c l.size J_left NoStyle c
  end
  | PortLabel (prange, ptype) ->
     let pcolor = match prange with
       | Glabel -> Green
       | Hlabel -> Red in
     svg_text ~kolor:pcolor line l.orient l.c l.size J_left NoStyle c


let parse_sheet_line line context canevas =
  match (String.get line 0) with
  | 'F' -> (context, parse_sheet_field ~context line canevas)
  | 'S' -> parse_sheet_rect ~context line (context,canevas)
  | 'U' -> context, canevas
  | _ -> (Printf.printf "unknown sheet line (%s)" line; context,canevas)

let parse_body_line (c,canevas) line =
  if (String.compare line "$Comp" == 0) then
    ComponentContext, canevas
  else if (String.compare (String.sub line 0 4) "Wire" == 0) then
    WireContext, canevas
  else if (String.compare (String.sub line 0 6) "NoConn" == 0) then
    BodyContext, parse_noconn_line line canevas
  else if (String.length line > 10) && (String.compare (String.sub line 0 10) "Connection" == 0) then
    BodyContext, parse_conn_line line canevas
  else if (String.compare line "$Sheet" == 0) then
    SheetContext None, canevas
  else if (String.length line > 5) && (String.compare (String.sub line 0 4) "Text" == 0) then
    parse_text_line line (c,canevas)
  else
    BodyContext, canevas

let parse_line line (c,canevas) =
  match c with
  | ComponentContext ->
     if (String.compare line "$EndComp" == 0) then
       (BodyContext, canevas)
     else
         (ComponentContext, parse_component_line line canevas)
  | BodyContext ->
     parse_body_line (c,canevas) line
  | WireContext ->
     BodyContext, (parse_wire_line line canevas)
  | SheetContext sc ->
     if (String.compare line "$EndSheet" == 0) then
       (BodyContext, canevas)
     else
       let nsc, o = parse_sheet_line line sc canevas in
       SheetContext nsc, o
  | TextContext sc ->
     match sc with
     |None -> failwith "TextContext without definition!"
     |Some v -> (BodyContext, print_text_line line v canevas)


let output_context (ctxt, canevas) oc =
  svg_write oc canevas
