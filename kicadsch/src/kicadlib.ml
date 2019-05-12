open KicadSch_sigs

module MakePainter (P : Painter) : CompPainter with type drawContext := P.t =
struct
  type relcoord = RelCoord of int * int

  type circle = {center: relcoord; radius: int}

  type pin_orientation = P_L | P_R | P_U | P_D

  type pin_tag = string * size

  type pin =
    { name: pin_tag
    ; number: pin_tag
    ; length: size
    ; contact: relcoord
    ; orient: pin_orientation }

  type primitive =
    | Field
    | Polygon of int * relcoord list
    | Circle of int * circle
    | Pin of pin
    | Text of {c: relcoord; text: string; s: size}
    | Arc of
        { s: size
        ; radius: int
        ; sp: relcoord
        ; ep: relcoord
        ; center: relcoord }

  type elt = {parts: int; prim: primitive}

  type component =
    { names: string list
    ; draw_pnum: bool
    ; draw_pname: bool
    ; multi: bool
    ; graph: elt list }

  let pin_orientation_of_string = function
    | "L" ->
        P_L
    | "R" ->
        P_R
    | "U" ->
        P_U
    | "D" ->
        P_D
    | s ->
        failwith ("pin orientation mismatch " ^ s)

  module Lib : Hashtbl.S with type key := string = Hashtbl.Make (struct
    type t = string

    let equal = String.equal

    let get_i s n = int_of_char s.[n]

    let hash s =
      let rec build_hash h i =
        if i < 0 then h else build_hash ((h * 47) + get_i s i) (i - 1)
      in
      build_hash 0 (String.length s - 1)
  end)

  type t = component Lib.t * component option * elt list

  type drawContext = P.t

  let lib () : t = (Lib.create 256, None, [])

  open Schparse

  let parse_def =
    create_lib_parse_fun ~name:"component header"
      ~regexp_str:"DEF %s %s 0 %d %[YN] %[YN] %d %[FL] %[NP]"
      ~processing:(fun name _ _ dpnum dpname unit_count _ _ ->
        let draw_pnum = dpnum.[0] = 'Y' in
        let draw_pname = dpname.[0] = 'Y' in
        let nname =
          if name.[0] = '~' then String.sub name 1 (String.length name - 1)
          else name
        in
        let multi = if unit_count = 1 then false else true in
        Some (nname, draw_pnum, draw_pname, multi) )

  (** Parsing component drawing primitives **)

  (** Parse a poly line
      P Nb parts convert thickness x0 y0 x1 y1 xi yi cc
   **)
  let rec make_double ol il =
    match il with
    | [] ->
        ol
    | [_] ->
        failwith "make double: odd number of coords!"
    | x :: y :: tl ->
        make_double (RelCoord (x, y) :: ol) tl

  let parse_integers = parse_list " %d "

  let parse_Poly =
    create_lib_parse_fun ~name:"polygon" ~regexp_str:"P %d %d %d %d %s@@"
      ~processing:(fun _ parts _ thickness remainder ->
        let coords = List.rev (parse_integers remainder) in
        let finish = remainder.[String.length remainder - 1] = 'F' in
        let coord_list = make_double [] coords in
        let corner_list =
          if finish then
            match coord_list with
            | [_] | [] ->
                coord_list
            | c :: _ ->
                c :: List.rev coord_list
          else List.rev coord_list
        in
        Some {parts; prim= Polygon (thickness, corner_list)} )

  let parse_rect =
    create_lib_parse_fun ~name:"rectangle"
      ~regexp_str:"S %d %d %d %d %d %d %d %s"
      ~processing:(fun x1 y1 x2 y2 parts _ thickness _ ->
        try
          let c1 = RelCoord (x1, y1) in
          let c2 = RelCoord (x2, y2) in
          let rect_poly = [c1; RelCoord (x1, y2); c2; RelCoord (x2, y1); c1] in
          Some {parts; prim= Polygon (thickness, rect_poly)}
        with _ -> None )

  let parse_circle =
    create_lib_parse_fun ~name:"circle" ~regexp_str:"C %d %d %d %d %d %d"
      ~processing:(fun x y radius parts _ width ->
        try
          let center = RelCoord (x, y) in
          Some {parts; prim= Circle (width, {center; radius})}
        with _ -> None )

  let parse_pin =
    create_lib_parse_fun ~name:"pin"
      ~regexp_str:"X %s %s %d %d %d %[RLUD] %d %d %d %d %s %s"
      ~processing:(fun nm nb x y sz o nm_sz nb_sz parts _ _ c ->
        if String.length c = 0 || c.[0] != 'N' then
          try
            let contact = RelCoord (x, y) in
            let length = Size sz in
            let orient = pin_orientation_of_string o in
            let name = (nm, Size nm_sz) in
            let number = (nb, Size nb_sz) in
            Some {parts; prim= Pin {name; number; length; contact; orient}}
          with _ -> None
        else Some {parts= -1; prim= Field} )

  let parse_alias =
    create_lib_parse_fun ~name:"ALIAS" ~regexp_str:"ALIAS %s@@"
      ~processing:(fun sp ->
        Some (parse_list ~cond:(fun s -> String.length s > 0) " %s " sp) )

  let parse_text =
    create_lib_parse_fun ~name:"Text" ~regexp_str:"T %d %d %d %d %d %d %d %s"
      ~processing:(fun _ x y sz _ parts _ text ->
        let c = RelCoord (x, y) in
        let s = Size sz in
        Some {parts; prim= Text {c; text; s}} )

  let parse_arc =
    create_lib_parse_fun ~name:"Arc"
      ~regexp_str:"A %d %d %d %d %d %d %d %d %s %d %d %d %d"
      ~processing:(fun x y radius _ _ parts _ sz _ spx spy epx epy ->
        let center = RelCoord (x, y) in
        let sp = RelCoord (spx, spy) in
        let ep = RelCoord (epx, epy) in
        let s = Size sz in
        Some {parts; prim= Arc {sp; ep; s; radius; center}} )

  let parse_line line =
    if String.length line > 0 then (
      match line.[0] with
      | 'A' -> (
        match parse_arc line with
        | Some a ->
            a
        | None ->
            failwith ("Error parsing arc " ^ line) )
      | 'P' -> (
        match parse_Poly line with
        | Some p ->
            p
        | None ->
            failwith ("Error parsing poly " ^ line) )
      | 'S' -> (
        match parse_rect line with
        | Some p ->
            p
        | None ->
            failwith ("Error parsing rectangle " ^ line) )
      | 'C' -> (
        match parse_circle line with
        | Some c ->
            c
        | None ->
            failwith ("Error parsing circle " ^ line) )
      | 'F' ->
          {parts= -1; prim= Field}
      | 'X' -> (
        match parse_pin line with
        | Some p ->
            p
        | None ->
            failwith ("Error parsing pin :" ^ line) )
      | 'T' -> (
        match parse_text line with
        | Some t ->
            t
        | None ->
            failwith ("Error parsing pin :" ^ line) )
      | ' ' | '$' ->
          {parts= -1; prim= Field}
      | _ ->
          Printf.printf "throwing away line '%s'\n" line ;
          {parts= -1; prim= Field} )
    else {parts= -1; prim= Field}

  let fix_illegal_chars name =
    String.map (function '/' | ':' -> '_' | c -> c) name

  let append_lib line (lib, comp_option, acc) =
    match comp_option with
    | None ->
        if
          String.length line > 3
          && String.compare (String.sub line 0 3) "DEF" = 0
        then
          match parse_def line with
          | Some (name, draw_pnum, draw_pname, multi) ->
              let new_comp =
                {names= [name]; draw_pnum; draw_pname; multi; graph= []}
              in
              (lib, Some new_comp, [])
          | None ->
              failwith ("could not parse component definition " ^ line)
        else (lib, None, [])
    | Some comp ->
        if String.compare line "DRAW" = 0 || String.compare line "ENDDRAW" = 0
        then (lib, comp_option, acc)
        else if String.compare line "ENDDEF" = 0 then (
          let comp = {comp with graph= List.rev acc} in
          List.iter
            (fun name -> Lib.replace lib (fix_illegal_chars name) comp)
            comp.names ;
          (lib, None, []) )
        else if
          String.length line > 6
          && String.compare (String.sub line 0 5) "ALIAS" = 0
        then
          match parse_alias line with
          | None ->
              failwith (Printf.sprintf "ALIAS line %s parse error\n" line)
          | Some name_list ->
              ( lib
              , Some {comp with names= List.rev_append comp.names name_list}
              , acc )
        else
          let prim = parse_line line in
          (lib, comp_option, prim :: acc)

  let ( +$ ) (Coord (x1, y1)) (RelCoord (x2, y2)) = Coord (x1 + x2, y1 + y2)

  let ( *$ ) ((a, b), (c, d)) (RelCoord (x, y)) =
    RelCoord ((a * x) + (b * y), (c * x) + (d * y))

  let rotate (origin : coord) (rotation : transfo) (relpoint : relcoord) :
      coord =
    origin +$ (rotation *$ relpoint)

  let rec plot_poly rotfun thickness points ctx =
    match points with
    | [] | [_] ->
        ctx
    | c1 :: c2 :: tl ->
        let c1' = rotfun c1 in
        let c2' = rotfun c2 in
        plot_poly rotfun thickness (c2 :: tl) (P.paint_line c1' c2' ctx)

  let plot_pin rotfun {name; number; length; contact; orient} c ctx =
    let (RelCoord (x, y)) = contact in
    let (Size delta) = length in
    let sc =
      match orient with
      | P_R ->
          RelCoord (x + delta, y)
      | P_L ->
          RelCoord (x - delta, y)
      | P_U ->
          RelCoord (x, y + delta)
      | P_D ->
          RelCoord (x, y - delta)
    in
    let (Coord (nxsc, nysc) as new_sc) = rotfun sc in
    let (Coord (nx, ny) as new_contact) = rotfun contact in
    let new_J, new_orient =
      if nx > nxsc then (J_right, Orient_H)
      else if nx < nxsc then (J_left, Orient_H)
      else if ny > nysc then (J_top, Orient_V)
      else (J_bottom, Orient_V)
    in
    let name_text, name_size = name in
    let pin_text, pin_size = number in
    let pin_ctx = P.paint_line new_sc new_contact ctx in
    let pname_ctx =
      if c.draw_pname && String.compare "~" name_text <> 0 then
        P.paint_text name_text new_orient new_sc name_size new_J NoStyle
          pin_ctx
      else pin_ctx
    in
    if c.draw_pnum && String.compare "~" pin_text <> 0 then
      P.paint_text pin_text new_orient new_contact pin_size new_J NoStyle
        pname_ctx
    else pname_ctx

  let plot_elt rotfun comp part ctx {parts; prim} =
    if parts = 0 || parts = part then
      match prim with
      | Polygon (t, pts) ->
          plot_poly rotfun t pts ctx
      | Circle (_, {center; radius}) ->
          P.paint_circle (rotfun center) radius ctx
      | Field ->
          ctx
      | Pin p ->
          plot_pin rotfun p comp ctx
      | Text {c; text; s} ->
          P.paint_text text Orient_H (rotfun c) s J_left NoStyle ctx
      | Arc {radius; sp; ep; center; _} ->
          P.paint_arc (rotfun center) (rotfun sp) (rotfun ep) radius ctx
    else ctx

  exception Component_Not_Found of string

  let plot_comp (lib, _, _) comp_name part rotation origin (ctx : drawContext)
      =
    let rot = rotate rotation origin in
    let thecomp =
      try Lib.find lib (fix_illegal_chars comp_name)
      with _ -> raise (Component_Not_Found comp_name)
    in
    ( List.fold_left (plot_elt rot thecomp part) ctx thecomp.graph
    , thecomp.multi )
end
