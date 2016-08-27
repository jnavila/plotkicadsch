open KicadSch_sigs
module MakePainter (P: Painter): (CompPainter with type drawContext=P.t) =
struct
  type circle = {center: coord; radius: int}
  type pin_orientation = P_L | P_R | P_U | P_D
  type pin_tag = (string * size)
  type pin = {
      name: pin_tag;
      number: pin_tag;
      length: size;
      contact: coord;
      orient: pin_orientation}

  type primitive =
    | Field
    | Polygon of int * (coord list)
    | Circle of int * circle
    | Pin of pin

  type component = {
      name: string;
      graph : primitive list
    }

  let pin_orientation_of_string = function
    | "L" -> P_L
    | "R" -> P_R
    | "U" -> P_U
    | "D" -> P_D
    | s -> failwith ("pin orientation mismatch "  ^ s)

  let create_lib_parse_fun = Schparse.create_lib_parse_fun

  module Lib : (Hashtbl.S with type key:=string) = Hashtbl.Make(
    struct
      type t = string
      let equal = (=)
      let get_i s n = int_of_char ( String.get s n )
      let hash s =
        let rec build_hash h i =
          if i <0 then
            h
          else
            build_hash (h*47 + (get_i s i)) (i-1)
        in build_hash 0 (String.length s - 1)
    end)

  type t = component Lib.t
  type drawContext = P.t

  let lib () : t = Lib.create 256

  let parse_def = create_lib_parse_fun
    ~name:"component header"
    ~regexp_str:"DEF ([^ ]+) ([^ ]+) 0 ([\\d-]+) (Y|N) (Y|N) ([\\d-]+) (F|L) (N|P)"
    ~processing:
    (fun sp ->
       Some {name=sp.(1); graph = []}
    )

  (** Parsing component drawing primitives **)

  (** Parse a poly line
      P Nb parts convert thickness x0 y0 x1 y1 xi yi cc
   **)
  let rec draw_lines coords ctx =
    match coords with
    | []  | [_]-> ctx
    | c1::c2::tl -> draw_lines tl (P.paint_line c1 c2 ctx)

  let rec make_double ol il =
    match il with
    | [] -> ol
    | [_] -> failwith "make double: odd number of coords!"
    | x::y::tl -> make_double ((Coord (x,y))::ol) tl

  let parse_Poly =
    create_lib_parse_fun
      ~name:"polygon"
      ~regexp_str:"P ([\\d]+) ([\\d]+) (0|1|2) ([\\d]+) (([\\d-]+ [\\d-]+ )+)N?F?"
    ~processing:
    (fun sp ->
      let thickness = int_of_string sp.(3) in
      let coords_str = Str.split (Str.regexp " +") sp.(5) in
      let coords = List.map int_of_string coords_str in
      let coord_list = make_double [] coords in
      Some (Polygon (thickness, List.rev coord_list))
    )

  let parse_rect =
    create_lib_parse_fun
      ~name: "rectangle"
      ~regexp_str: "S ([\\d-]+) ([\\d-]+) ([\\d-]+) ([\\d-]+) (0|1) ([\\d-]+) ([\\d-]+) (N)?(F)?"
      ~processing:
      ( fun sp ->
        try
          let x1 = int_of_string sp.(1) in
          let y1 = int_of_string sp.(2) in
          let x2 = int_of_string sp.(3) in
          let y2 = int_of_string sp.(4) in
          let c1 = Coord (x1, y1) in
          let c2 = Coord (x2, y2) in
          let thickness = int_of_string sp.(6) in
          let rect_poly = [ c1 ; Coord (x1, y2) ; c2 ; Coord (x2, y1) ; c1 ] in
          Some (Polygon (thickness, rect_poly))
        with _ -> None
      )

  let parse_circle =
    create_lib_parse_fun
      ~name: "circle"
      ~regexp_str: "C ([\\d-]+) ([\\d-]+) ([\\d-]+) (0|1) ([\\d-]+) ([\\d-]+) (N|NF)"
      ~processing:
      ( fun sp ->
        try
          let x = int_of_string sp.(1) in
          let y = int_of_string sp.(2) in
          let center = Coord (x, y) in
          let radius = int_of_string sp.(3) in
          let width = int_of_string sp.(6) in
          Some (Circle (width, {center; radius}))
        with _ -> None
      )

  let parse_pin =
          create_lib_parse_fun
      ~name: "pin"
      ~regexp_str: "X ([^ ]+) ([^ ]+) ([\\d-]+) ([\\d-]+) ([\\d-]+) (R|L|U|D) ([\\d]+) ([\\d]+) ([\\d]+) (0|1|2)"
      ~processing:
      ( fun sp ->
        try
          let x = int_of_string sp.(3) in
          let y = int_of_string sp.(4) in
          let contact = Coord (x, y) in
          let length = Size (int_of_string sp.(5)) in
          let orient = pin_orientation_of_string sp.(6) in
          let name = sp.(1), Size ((int_of_string sp.(7))) in
          let number = sp.(2), (Size (int_of_string sp.(8))) in
          Some (Pin {name;number;length;contact;orient})
        with _ -> None
      )

  let parse_line (line: string) =
    match (String.get line 0) with
    |'P' ->
      begin
        match parse_Poly line with
        | Some p -> p
        | None -> failwith ("Error parsing poly " ^ line)
      end
    |'S' ->
      begin
        Printf.printf "Parse Rectangle\n";
        match parse_rect line with
        | Some p -> p
        | None -> failwith ("Error parsing rectangle " ^ line)
      end
    |'C' ->
      begin
        match parse_circle line with
        | Some c -> c
        | None -> failwith ("Error parsing circle " ^ line)
      end
    |'F' -> Field
    |'X' ->
      begin
        match parse_pin line with
        |Some p -> p
        |None -> failwith ("Error parsing pin :" ^ line)
      end
    | _ -> Printf.printf "throwing away line '%s'\n" line; Field

  let rec append_line ic lib comp_option acc =
    try
      let line = input_line ic in
      match comp_option with
      | None ->
         if (String.length line > 3) &&
              (String.compare (String.sub line 0 3) "DEF" = 0) then
           match parse_def line with
           | Some _ as thecomp-> append_line ic lib thecomp []
           | None -> failwith ("could not parse component definition " ^ line)
         else
           append_lib ic lib
      | Some comp ->
         if (String.compare line "ENDDEF" = 0) then
           let comp_elts = List.rev acc in
           (Lib.replace  lib comp.name {comp with graph=comp_elts});
           Printf.printf "added new component %s\n" comp.name;
           append_lib ic lib
         else
           let prim = parse_line line in
           append_line ic lib comp_option (prim::acc)
    with
      End_of_file -> lib

  and append_lib ic lib = append_line ic lib None []

  let ( +$) (Coord(x1, y1)) (Coord(x2, y2)) = Coord((x1 + x2), (y1 + y2))
  let ( *$) ((a,b),(c,d)) (Coord(x, y)) = Coord((a * x + b * y), (c * x + d * y))

  let rotate (origin: coord) (rotation:transfo) (relpoint:coord) =
     origin +$ rotation *$ relpoint

  let rec plot_poly rotfun thickness points ctx =
    match points with
    | []  | [_]-> ctx
    | c1::c2::tl ->
       let c1' = rotfun c1 in
       let c2' = rotfun c2 in
       plot_poly rotfun thickness (c2::tl) (P.paint_line c1' c2' ctx)

  let plot_pin rotfun {name;number;length;contact;orient} ctx =
    let Coord (x,y) = contact in
    let Size delta = length in
    let sc = Coord(match orient with
      | P_R -> (x+delta), y
      | P_L -> (x-delta), y
      | P_U -> x, (y+delta)
      | P_D -> x, (y-delta)) in
    let Coord (nxsc, nysc) as new_sc = rotfun sc in
    let Coord (nx, ny) as new_contact = rotfun contact  in
    let new_J, new_orient =
      if nx>nxsc then J_right, Orient_H
      else if nx<nxsc then J_left, Orient_H
      else if ny>nysc then J_top, Orient_V
      else J_bottom, Orient_V in
    let name_text, name_size = name in
    let pin_text, pin_size = number in
    ctx |>
      P.paint_line new_sc new_contact |>
      P.paint_text name_text new_orient new_sc name_size new_J NoStyle |>
      P.paint_text pin_text new_orient new_contact pin_size new_J NoStyle

  let plot_elt elt rotfun ctx =
    match elt with
    | Polygon (t, pts) -> plot_poly rotfun t pts ctx
    | Circle (w,{center;radius}) -> P.paint_circle (rotfun center) radius ctx
    | Field -> ctx
    | Pin p -> plot_pin rotfun p ctx

  let rec plot_elts rotfun compelts ctx =
    match compelts with
    | [] -> ctx
    | elt::tl -> plot_elts rotfun tl (plot_elt elt rotfun ctx)

  exception Component_Not_Found of string

  let plot_comp lib comp_name rotation origin (ctx:P.t) =
    let () = Printf.printf "trying to plot component %s\n" comp_name in
    let rot : (coord -> coord) = rotate rotation origin in
    let thecomp =
      try
        Lib.find lib comp_name
      with _ -> raise (Component_Not_Found comp_name) in
    plot_elts rot thecomp.graph ctx
end
