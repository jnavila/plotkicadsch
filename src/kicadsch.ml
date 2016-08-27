module MakeSchPainter (P: KicadSch_sigs.Painter): KicadSch_sigs.SchPainter =
struct
  module CPainter = Kicadlib.MakePainter(P)

  open KicadSch_sigs

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

  type field = { text: string;
                 o: orientation;
                 co: coord;
                 s: size;
                 j: justify;
                 stl: style}

  type componentContext = {
      component: string option;
      unit: int option;
      origin: coord option;
      fields: field list }

  type schParseContext =
      BodyContext
    | WireContext
    | ComponentContext of componentContext
    | SheetContext of rect option
    | TextContext of label option

  type schContext = CPainter.t * schParseContext * P.t

  let initial_context () = CPainter.lib () , BodyContext, P.get_context ()

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

  (* Parsing a sch file *)

  (** This function generates a parsing function which outputs an 'a
      option Note that some lines may not yield any correct output, so
      the output is an option.
  let create_sch_parse_fun ~name ~regexp_str ~processing =
    let regexp = Pcre.regexp regexp_str in
    let parser ?context line c =
      try
        let sp = Pcre.extract ~rex:regexp line in
        processing context sp c
      with Not_found ->
        (Printf.printf "could not match %s (%s)\n" name line; c)
    in parser
  **)
  let parse_F =
    Schparse.create_parse_fun
    ~name:"Component F"
    ~regexp_str:"F [\\d-]+ \"([^\"]*)\" (H|V) ([\\d-]+) ([\\d-]+) ([\\d-]+)? +(0|1)(0|1)(0|1)(0|1) (L|R|C|B|T) (L|R|C|B|T)(I|N)(B|N)"
    ~extract_fun:
    (fun sp ->
      let co = Coord (int_of_string sp.(3), int_of_string sp.(4)) and
          o = orientation_of_string sp.(2) and
          s = Size (int_of_string sp.(5))and
          j = justify_of_string sp.(10) and
          stl = style_of_string (sp.(12), sp.(13)) and
          visible = if (String.get sp.(9) 0 = '0') then true else false in
      Some (visible, sp.(1), o, co, s, j, stl)
    )

  let parse_L =
    Schparse.create_parse_fun
      ~name: "Component L"
      ~regexp_str: "L ([-\\+\\w]+) ([-\\+\\w#\\d]+)"
      ~extract_fun:
      (fun sp ->
        Some (sp.(1), sp.(2))
      )

  let parse_P =
    Schparse.create_parse_fun
      ~name: "Component P"
      ~regexp_str: "P ([\\d-]+) ([\\d-]+)"
      ~extract_fun:
      (fun sp ->
        let x = int_of_string sp.(1) and
            y = int_of_string sp.(2) in
        Some (Coord (x, y)))

  let parse_U =
    Schparse.create_parse_fun
      ~name: "Component U"
      ~regexp_str: "U ([\\d-]+) (\\w+) (\\w+)"
      ~extract_fun:
      (fun sp ->
        let u = int_of_string sp.(1) in
        Some (u, sp.(2), sp.(3)))

  let parse_transfo =
    Schparse.create_parse_fun
      ~name: "Component transformation"
      ~regexp_str: "	(0|1|-1) +(0|1|-1) +(0|1|-1) +(0|1|-1) +"
      ~extract_fun:
      (fun sp ->
        let a= int_of_string sp.(1) and
            b= int_of_string sp.(2) and
            c= int_of_string sp.(3) and
            d= int_of_string sp.(4) in
        Some (a, b, c, d)
      )

  let draw_field (Coord (x0, y0)) ((a,b),(c,d)) context {text; o; co; s; j; stl} =
    let Coord (x, y) = co in
    let xrel = x - x0 and yrel = y - y0 in
    let x' = (a * xrel + b * yrel) + x0 in
    let y' = (c * xrel + d * yrel) + y0 in
    let o' =
      if a = 0 then
        match o with
        | Orient_H -> Orient_V
        | Orient_V -> Orient_H
      else
        o in
    P.paint_text text o' (Coord (x', y')) s j stl context

  let parse_component_line lib (line: string) (comp: componentContext) canevas =
    let first = String.get line 0 in
    match first with
    | 'F' ->
       let fields =
         parse_F
           line
           ~onerror: (fun () -> comp.fields)
           ~process: (fun (visible, text, o, co, s, j, stl) ->
             if visible then
               {text; o; co; s; j; stl}::comp.fields else comp.fields) in
       {comp with fields}, canevas
    | 'U' ->
       let unit =
         parse_U
           line
           ~onerror: (fun () ->  comp.unit)
           ~process: (fun (u, _, _) -> Some u ) in
       {comp with unit}, canevas
    | 'P' ->
       let origin =
         parse_P
           line
           ~onerror: (fun () -> comp.origin)
           ~process: (fun c -> Some c)
       in {comp with origin}, canevas
    | 'L' ->
       let component =
         parse_L
           line
           ~onerror: (fun () ->  comp.component)
           ~process: ( fun (s, _) -> Some s) in
       {comp with component}, canevas
    | '	' ->
       parse_transfo
         line
         ~onerror: ( fun () -> comp, canevas)
         ~process: (fun (a, b, c, d) ->
           let {component; unit; origin; fields} = comp in
           match component, unit, origin with
           | Some sym, Some unit, Some origin ->
              let transfo = ((a, b), (c, d)) in
              let canevas' = CPainter.plot_comp lib sym origin transfo canevas in
              let draw = draw_field origin transfo in
              comp, List.fold_left draw canevas' fields
           | _ ->
              (Printf.printf "cannot plot component with missing definitions !";
               comp, canevas))

    | _ -> comp, canevas

  let parse_wire_line = Schparse.create_parse_fun
    ~name:"Wire"
    ~regexp_str:"\\t([\\d-]+) +([\\d-]+) +([\\d-]+) +([\\d-]+)"
    ~extract_fun:
    (fun sp ->
      let c1 = Coord (int_of_string sp.(1), int_of_string sp.(2)) and
          c2 = Coord (int_of_string sp.(3), int_of_string sp.(4))
      in
      Some (c1, c2)
    )

  let parse_noconn_line = Schparse.create_parse_fun
    ~name:"NoConn"
    ~regexp_str:"NoConn ~ ([\\d-]+) +([\\d-]+)"
    ~extract_fun:
    (fun sp ->
      let x = int_of_string sp.(1) and y=int_of_string sp.(2) in
      Some (Coord (x,y))
    )

  let parse_conn_line = Schparse.create_parse_fun
    ~name:"Connection"
    ~regexp_str:"Connection ~ ([\\d-]+) +([\\d-]+)"
    ~extract_fun:
    (fun sp ->
      let x = int_of_string sp.(1) and y=int_of_string sp.(2) in
      Some (Coord (x,y))
    )

  let parse_sheet_field = Schparse.create_parse_fun
    ~name:"Sheet Field"
    ~regexp_str:"F(0|1) +\"([^\"]*)\" +([\\d-]+)"
    ~extract_fun:(fun sp ->
      let number= int_of_string sp.(1) and
          name = sp.(2) and
          size = int_of_string sp.(3) in
      Some (number, name, Size size))

  let parse_sheet_rect = Schparse.create_parse_fun
    ~name:"Sheet Rect"
    ~regexp_str:"S +([\\d-]+) +([\\d-]+) +([\\d-]+) +([\\d-]+)"
    ~extract_fun:(fun sp ->
      let c = Coord (int_of_string sp.(1), int_of_string sp.(2)) and
          dim = Coord (int_of_string sp.(3), int_of_string sp.(4))
      in
      Some {c;dim}
    )

  let parse_text_line = Schparse.create_parse_fun
    ~name:"Text header"
    ~regexp_str: "Text (GLabel|HLabel|Label|Notes) ([\\d-]+) ([\\d-]+) ([\\d-]+)    ([\\d-]+)   (~|UnSpc|3State|Output|Input)"
    ~extract_fun: (fun sp ->
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
      Some {c; size; orient; labeltype})

  (* Printing things *)

  let print_text_line line l c =
    match l.labeltype with
    | TextLabel t -> begin
      let pcolor = match t with
        | TextNote ->  Green
        | WireLabel -> Red in
      P.paint_text ~kolor:pcolor line l.orient l.c l.size J_left NoStyle c
    end
    | PortLabel (prange, ptype) ->
       let pcolor = match prange with
         | Glabel -> Green
         | Hlabel -> Red in
       P.paint_text ~kolor:pcolor line l.orient l.c l.size J_left NoStyle c

  let parse_sheet_line line context canevas =
    match (String.get line 0) with
    | 'F' ->
       context,
       (parse_sheet_field
          line
          ~onerror:(fun () -> canevas)
          ~process:(fun (number, name, (Size size as s)) ->
            match context with
            | Some {c=Coord (x, y); dim=Coord (dim_x, dim_y)} ->
               let y = if (number = 0) then y else y + dim_y + size in
               P.paint_text name Orient_H (Coord (x, y))  s J_left NoStyle canevas
            | None -> canevas))
    | 'S' ->
       parse_sheet_rect
         line
         ~onerror:(fun () -> context,canevas)
         ~process:(fun ({c;dim} as range) ->
           (Some range), (P.paint_rect c dim canevas))
    | 'U' -> context, canevas
    | _ -> (Printf.printf "unknown sheet line (%s)" line; context,canevas)

  let parse_body_line (lib, c,canevas) line =
    if (String.compare line "$Comp" = 0) then
      (ComponentContext {component=None; unit=None; origin=None;fields= []}), canevas
    else if (String.compare (String.sub line 0 4) "Wire" = 0) then
      WireContext, canevas
    else if (String.compare (String.sub line 0 6) "NoConn" = 0) then
      BodyContext, parse_noconn_line
                     line
                     ~onerror:(fun () -> canevas)
                     ~process:(fun (Coord (x,y)) ->
                       let delta = 20 in
                       canevas |>
                         P.paint_line (Coord (x - delta, y - delta)) (Coord (x + delta, y + delta)) |>
                         P.paint_line (Coord (x - delta, y + delta)) (Coord (x + delta, y - delta)))

    else if (String.length line > 10) && (String.compare (String.sub line 0 10) "Connection" = 0) then
      BodyContext, parse_conn_line
                     line
                     ~onerror:(fun () -> canevas)
                     ~process:(fun (Coord (x,y)) ->
                       let delta = 10 in
                       P.paint_circle ~fill:Black (Coord (x,y)) delta canevas)
    else if (String.compare line "$Sheet" = 0) then
      SheetContext None, canevas
    else if (String.length line > 5) && (String.compare (String.sub line 0 4) "Text" = 0) then
      TextContext (parse_text_line
                     line
                     ~onerror:(fun () -> None)
                     ~process:(fun c -> Some c)), canevas
    else
      BodyContext, canevas

  let parse_line line (lib, c,canevas) =
    match c with
    | ComponentContext comp ->
       if (String.compare line "$EndComp" = 0) then
         (lib, BodyContext, canevas)
       else
         let comp, canevas = parse_component_line lib line comp canevas in
         (lib, (ComponentContext comp), canevas)
    | BodyContext ->
       let c, canevas = parse_body_line (lib, c,canevas) line
       in lib, c, canevas
    | WireContext ->
       lib, BodyContext, (parse_wire_line
                            line
                            ~onerror: (fun () -> canevas)
                            ~process: (fun (c1, c2) -> P.paint_line c1 c2 canevas))
    | SheetContext sc ->
       if (String.compare line "$EndSheet" = 0) then
         (lib, BodyContext, canevas)
       else
         let nsc, o = parse_sheet_line line sc canevas in
         lib, SheetContext nsc, o
    | TextContext sc ->
       match sc with
       |None -> failwith "TextContext without definition!"
       |Some v -> (lib, BodyContext, print_text_line line v canevas)

  let output_context (lib, ctxt, canevas) oc =
    P.write oc canevas

  let add_lib filename (lib, ctxt, canevas) =
    Printf.printf "parsing lib %s\n" filename;
    let ic = open_in filename in
    (CPainter.append_lib ic lib), ctxt, canevas

end
