open KicadDefs
(* open KicadSch_sigs *)
open KicadLib_sigs

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

type t = component Lib.t * component option * elt list
let lib () = (Lib.create 256, None, [])

let get_comp_lib (lib, _, _) = lib

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
          ((add_component comp lib), None, []) )
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
