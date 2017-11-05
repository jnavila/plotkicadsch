open KicadSch_sigs
module SvgPainter = SvgPainter
module Sigs=KicadSch_sigs
module MakeSchPainter (P: Painter): (SchPainter with type painterContext := P.t) =
struct
  module CPainter = Kicadlib.MakePainter(P)

  open KicadSch_sigs

  type rect = { c:coord ; dim:coord }
  type portrange = Glabel | Hlabel
  type labeluse = WireLabel | TextNote
  type porttype = UnSpcPort | ThreeStatePort | OutputPort | InputPort | NoPort | BiDiPort
  type linetype = Wire | Bus | Line | WireEntry | BusEntry

  type labeltype =
    | PortLabel of portrange * porttype
    | TextLabel of labeluse

  type label = { c: coord; size: size; orient: justify; labeltype: labeltype }

  type field = { text: string;
                 o: orientation;
                 co: coord;
                 s: size;
                 j: justify;
                 stl: style}

  type componentContext = {
      component: string option;
      unitnr: int option;
      origin: coord option;
      fields: field list }

  type bitmapContext = { pos:coord option; scale:float option; data: Buffer.t option}

  type schParseContext =
      BodyContext
    | DescrContext of coord
    | WireContext of linetype
    | ComponentContext of componentContext
    | SheetContext of rect option
    | TextContext of label option
    | BitmapContext of bitmapContext

  type schContext = CPainter.t * schParseContext * P.t

  let initial_context () = CPainter.lib () , BodyContext, P.get_context ()

  let swap_type = function
    | UnSpcPort | ThreeStatePort  | NoPort | BiDiPort as p -> p
    | OutputPort -> InputPort
    | InputPort -> OutputPort

  let porttype_of_string = function
    | "U"| "UnSpc" -> UnSpcPort
    | "T"| "3State" -> ThreeStatePort
    | "O"| "Output" -> OutputPort
    | "I"| "Input" -> InputPort
    | "B"| "BiDi" -> BiDiPort
    | "~" -> NoPort
    |   _ as s -> ignore (Printf.printf "unknown port type %s\n" s); NoPort

  let justify_of_string s =
    match String.get s 0 with
    | 'L'| '0' -> J_left
    | 'R'| '2' -> J_right
    | 'C' -> J_center
    | 'B' | '1' -> J_bottom
    | 'T' | '3' -> J_top
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

  let orientation_of_justify = function
    | J_left | J_right | J_center -> Orient_H
    | J_top | J_bottom -> Orient_V

  (* Parsing a sch file *)

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
            visible = (String.get sp.(9) 0 == '0') && not (String.equal "~" sp.(1)) in
        Some (visible, sp.(1), o, co, s, j, stl)
    )

  let parse_L =
    Schparse.create_parse_fun
      ~name: "Component L"
      ~regexp_str: "L ([^ ]+) +([^ ]+)"
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
      ~regexp_str: "	([\\d-]+) +([\\d-]+) +([\\d-]+)( +([\\d-]+))?"
      ~extract_fun:
      (fun sp ->
        let a = int_of_string sp.(1) and
            b = int_of_string sp.(2) and
            c = int_of_string sp.(3) and
            d = try
                  int_of_string sp.(5)
              with _ -> -5000 in
        Some (a, b, c, d)
      )
  let swap_justify = function
    | J_left -> J_right
    | J_center -> J_center
    | J_right -> J_left
    | J_bottom -> J_top
    | J_top -> J_bottom


  let draw_field (Coord (x0, y0)) ((a,b),(c,d)) context {text; o; co; s; j; stl} =
    let Coord (x, y) = co in
    let xrel = x - x0 and yrel = y - y0 in
    let x' = (a * xrel + b * yrel) + x0 in
    let y' = (c * xrel + d * yrel) + y0 in
    let o' =
      if a = 0 then (* this is a ±90° rotation matrix *)
        match o with
        | Orient_H -> Orient_V
        | Orient_V -> Orient_H
      else
        o in
    let j' = match o' with
      | Orient_H -> if ((a = (-1)) or (b = (-1))) then (swap_justify j) else j
      | Orient_V -> if ((c = (1)) or (d = (-1))) then (swap_justify j) else j in
    P.paint_text text o' (Coord (x', y')) s j' stl context

  let right_arrow = "\xE2\x96\xB6"
  let left_arrow = "\xE2\x97\x80"
  let diamond = "\xE2\x97\x86"
  let square = "\xE2\x97\xBC"

  let decorate_port_name name ptype justif =
    let port_char = match ptype, justif with
      |  UnSpcPort,_ | NoPort,_ -> ""
      | ThreeStatePort,_  | BiDiPort,_ -> diamond
      | OutputPort,(J_left|J_top) | InputPort, (J_right | J_bottom) -> left_arrow
      | OutputPort, (J_right | J_bottom) | InputPort, (J_left | J_top) -> right_arrow
      | _, J_center -> square
    in
    match justif with
      | J_left | J_top -> port_char ^ name
      | J_right |J_bottom -> name ^ port_char
      | J_center -> name

  let draw_port ?(kolor=Black) name ptype justif (Coord (x,y)) (Size l as s) canevas =
    let new_port_name = decorate_port_name name ptype justif in
    let orient = orientation_of_justify justif in
    let j = justif in
    let _ = kolor in
    let c = match orient with
    | Orient_H -> Coord (x,y+l/4)
    | Orient_V -> Coord (x+l/4,y) in
    P.paint_text new_port_name orient c s j NoStyle canevas

  let parse_component_line lib (line: string) (comp: componentContext) canevas =
    let first = String.get line 0 in
    match first with
    | 'F' ->
       let nc =
         parse_F
           line
           ~onerror: (fun () -> comp)
           ~process: (fun (visible, text, o, co, s, j, stl) ->
             if visible then
               {comp with fields={text; o; co; s; j; stl}::comp.fields} else comp) in
       nc, canevas
    | 'U' ->
       let nc =
         parse_U
           line
           ~onerror: (fun () ->  comp)
           ~process: (fun (u, _, _) -> {comp with unitnr=Some u} ) in
       nc, canevas
    | 'P' ->
       let nc =
         parse_P
           line
           ~onerror: (fun () -> comp)
           ~process: (fun c -> {comp with origin=Some c})
       in nc, canevas
    | 'L' ->
       let nc =
         parse_L
           line
           ~onerror: (fun () ->  comp)
           ~process: ( fun (s, _) -> {comp with component=Some s}) in
       nc, canevas
    | '	' ->
       parse_transfo
         line
         ~onerror: ( fun () -> comp, canevas)
         ~process: (fun (a, b, c, d) ->
           if d > -5000 then
             let {component; unitnr; origin; fields} = comp in
             match component, unitnr, origin with
             | Some sym, Some unit, Some origin ->
                let transfo = ((a, b), (c, d)) in
                let canevas' = CPainter.plot_comp lib sym unit origin transfo canevas in
                let draw = draw_field origin transfo in
                comp, List.fold_left draw canevas' fields
             | _ ->
                (Printf.printf "cannot plot component with missing definitions !";
                 comp, canevas)
           else comp,canevas)
    | _ -> (ignore(Printf.printf "ignored %s\n" line);
          comp, canevas)

  let parse_wire_wire =
    Schparse.create_parse_fun
      ~name:"Wire header"
      ~regexp_str: "(Wire|Entry) (Wire|Bus|Notes) (Line|Note)"
      ~extract_fun:
      (fun sp ->
        match sp.(1), sp.(2), sp.(3) with
        | "Wire", "Wire", "Line" -> Some Wire
        | "Wire", "Bus", "Line"  -> Some Bus
        | "Wire", "Wire", "Note" -> Some Line
        | "Entry", "Wire", "Line"  -> Some WireEntry
        | "Entry", "Bus", "Line"   -> Some BusEntry
        | _, _, _ -> None
      )

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

  let parse_sheet_field01 = Schparse.create_parse_fun
    ~name:"Sheet Field 0 or 1"
    ~regexp_str:"F(0|1) +\"([^\"]*)\" +([\\d-]+)"
    ~extract_fun:(fun sp ->
      let number= int_of_string sp.(1) and
          name = sp.(2) and
          size = int_of_string sp.(3) in
      Some (number, name, Size size))

  let parse_sheet_other_fields =
    Schparse.create_parse_fun
      ~name: "Sheet generic field"
      ~regexp_str: "F([\\d]+) +\"([^\"]*)\" (I|O|B|T|U) +(R|L|T|B) +([\\d-]+) +([\\d-]+) +([\\d]+)"
      ~extract_fun:(fun sp ->
        let name = sp.(2) in
        let ptype = porttype_of_string sp.(3) in
        let justif = justify_of_string sp.(4) in
        let c = Coord ((int_of_string sp.(5)),int_of_string sp.(6)) in
        let s = Size (int_of_string sp.(7)) in
        Some (name, ptype, justif, c, s)
      )

  let parse_sheet_field =
    Schparse.create_parse_fun
      ~name: "detect sheet field"
      ~regexp_str:"F([\\d]+)"
      ~extract_fun:(fun sp ->
        Some(int_of_string sp.(1))
      )

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
    ~regexp_str: "Text (GLabel|HLabel|Label|Notes) +([\\d-]+) +([\\d-]+) +([\\d-]+) +([\\d-]+) +(~|UnSpc|3State|Output|Input|BiDi)"
    ~extract_fun: (fun sp ->
      let c = Coord (int_of_string sp.(2), int_of_string sp.(3)) and
          j = justify_of_string sp.(4) and
          size = Size (int_of_string sp.(5)) in
      let labeltype, orient =
        match sp.(1) with
        | "GLabel" -> PortLabel (Glabel, porttype_of_string sp.(6)), swap_justify j
        | "HLabel" -> PortLabel (Hlabel, porttype_of_string sp.(6)), swap_justify j
        | "Label" -> TextLabel WireLabel, j
        | "Notes" -> TextLabel TextNote, j
        | _ -> TextLabel TextNote, j in
      Some {c; size; orient; labeltype})

  let parse_descr_header =
    Schparse.create_parse_fun
      ~name: "Descr header"
      ~regexp_str: "Descr ([^ ]+) +(\\d+) +(\\d+)"
      ~extract_fun: (fun sp ->
        let x = int_of_string sp.(2) in
        let y = int_of_string sp.(3) in
        Some (sp.(1), Coord (x,y))
      )

  let parse_descr_body =
    Schparse.create_parse_fun
      ~name: "Description line"
      ~regexp_str: "([^ ]+) \"?([^\"]*)\"?"
      ~extract_fun: (fun sp ->
        Some (sp.(1), sp.(2))
      )

  let parse_bm_pos =
    Schparse.create_parse_fun
      ~name: "Bitmap Pos"
      ~regexp_str: "Pos ([\\d-]+) ([\\d-]+)"
      ~extract_fun: (fun sp ->
        Some (Coord (int_of_string sp.(1), int_of_string sp.(2))))

  let parse_bm_scale =
    Schparse.create_parse_fun
      ~name: "Bitmap Scale"
      ~regexp_str: "Scale ([\\d-.]+)"
      ~extract_fun: (fun sp ->
        Some (float_of_string sp.(1)))

  (* Printing things *)

  let print_text_line line l c =
    match l.labeltype with
    | TextLabel t -> begin
      let pcolor = match t with
        | TextNote ->  Green
        | WireLabel -> Red in
      let Size s = l.size in
      let Coord (x,y) = l.c in
      let paint_line c' (line_index,l') =
        P.paint_text ~kolor:pcolor l' (orientation_of_justify l.orient) (Coord(x, (y-line_index*s))) l.size l.orient NoStyle c' in
      let lines = Str.split (Str.regexp "\\\\n") line in
      let ilines = List.rev lines in
      List.fold_left paint_line c (List.mapi (fun i l -> (i,l)) ilines)
    end
    | PortLabel (prange, ptype) ->
       let pcolor = match prange with
         | Glabel -> Green
         | Hlabel -> Red in
       let new_type = (swap_type ptype) in
       draw_port ~kolor:pcolor line new_type l.orient l.c l.size c

  let plot_page_frame (Coord (x, y)) canevas =
    let b_width = 100 in
    let f_width = 4000 in
    let bot_x = x - b_width in
    let bot_y = y - b_width in
    let frame_x = bot_x - f_width in
    canevas |>
      P.paint_rect (Coord (b_width, b_width)) (Coord (x -2*b_width, y - 2*b_width)) |>
      P.paint_rect (Coord (frame_x, bot_y - 150)) (Coord (f_width, 150)) |>
      P.paint_rect (Coord (frame_x, bot_y - 250)) (Coord (f_width, 100)) |>
      P.paint_rect (Coord (frame_x, bot_y - 550)) (Coord (f_width, 400))

  let plot_bitmap b context =
    match b.pos, b.scale, b.data with
    | Some p, Some s, Some d -> P.paint_image p s d context
    | _ -> context

  (* high level parsing *)

  let parse_sheet_line line context canevas =
    match (String.get line 0) with
    | 'F' ->
       context,
       (parse_sheet_field
          line
          ~onerror:(fun () -> canevas)
          ~process:(fun number ->
            if number < 2 then
              parse_sheet_field01
                line
                ~onerror:(fun () -> canevas)
                ~process:(fun (number, name, (Size size as s)) ->
                  match context with
                  | Some {c=Coord (x, y); dim=Coord (_, dim_y)} ->
                     let y = if (number = 0) then y else y + dim_y + size in
                     P.paint_text name Orient_H (Coord (x, y))  s J_left NoStyle canevas
                  | None -> canevas)
            else
              parse_sheet_other_fields
                line
                ~onerror:(fun () -> canevas)
                ~process:(fun (name, ptype, justif, c, s) ->
                draw_port name ptype justif c s canevas)
          )
       )
    | 'S' ->
       parse_sheet_rect
         line
         ~onerror:(fun () -> context,canevas)
         ~process:(fun ({c;dim} as range) ->
           (Some range), (P.paint_rect c dim canevas))
    | 'U' -> context, canevas
    | _ -> (Printf.printf "unknown sheet line (%s)" line; context,canevas)

  let starts_with str p =
    let len = String.length p in
    if String.length str < len then
      false
    else
      let rec comp_rec str p i =
        if String.get str i <> String.get p i then
          false
        else if i = len - 1 then
          true
        else comp_rec str p (i + 1) in
      comp_rec str p 0

  let parse_body_line (_, _,canevas) line =
    if (String.compare line "$Comp" = 0) then
      (ComponentContext {component=None; unitnr=None; origin=None;fields= []}), canevas
    else if (String.compare line "$Bitmap" = 0) then
      BitmapContext {pos=None;scale=None;data=None}, canevas
    else if starts_with line "$Descr" then
      parse_descr_header
                      line
                      ~onerror: (fun () -> BodyContext, canevas)
                      ~process: (fun (_, (Coord (x,y) as f_left)) ->
                        DescrContext (Coord ((x - 4000), (y - 100))), (plot_page_frame f_left (P.set_canevas_size x y canevas)))
    else if (starts_with line "Wire") || (starts_with line "Entry") then
      (parse_wire_wire
        line
        ~onerror: (fun () -> BodyContext)
        ~process: (fun lt -> WireContext lt))
      , canevas
    else if starts_with line "NoConn" then
      BodyContext, parse_noconn_line
                     line
                     ~onerror:(fun () -> canevas)
                     ~process:(fun (Coord (x,y)) ->
                       let delta = 20 in
                       canevas |>
                         P.paint_line (Coord (x - delta, y - delta)) (Coord (x + delta, y + delta)) |>
                         P.paint_line (Coord (x - delta, y + delta)) (Coord (x + delta, y - delta)))

    else if starts_with line "Connection" then
      BodyContext, parse_conn_line
                     line
                     ~onerror:(fun () -> canevas)
                     ~process:(fun (Coord (x,y)) ->
                       let delta = 10 in
                       P.paint_circle ~fill:Black (Coord (x,y)) delta canevas)
    else if (String.compare line "$Sheet" = 0) then
      SheetContext None, canevas
    else if starts_with line "Text" then
      TextContext (parse_text_line
                     line
                     ~onerror:(fun () -> None)
                     ~process:(fun c -> Some c)), canevas
    else
      BodyContext, canevas

  let parse_descr_line line (Coord (x,y)) canevas =
    parse_descr_body
      line
      ~onerror:(fun () -> canevas)
      ~process:(fun (field, content) ->
        let title_text content x y s =
          P.paint_text content Orient_H (Coord (x, y)) (Size s) J_left NoStyle canevas in
        match field with
        | "Sheet" -> title_text ("Page: " ^ content) x (y - 200) 50
        | "Title" -> title_text ("Title: "^ content) x (y - 50) 100
        | "Rev" -> title_text ("Rev: "^ content) (x + 3200) (y - 50) 100
        | "Date" -> title_text ("Date: " ^ content) (x + 500) (y -200) 50
        | "Comp"  -> title_text (content) (x + 1000) (y -200) 50
        | "Comment1"  -> title_text (content) (x) (y - 400) 50
        | "Comment2"  -> title_text (content) (x + 2000) (y - 400) 50
        | "Comment3"  -> title_text (content) (x) (y - 300) 50
        | "Comment4"  -> title_text (content) (x + 2000) (y - 300) 50
        | _ -> canevas)


  let append_bm_line data_opt line =
    match data_opt with
    | None -> failwith "not adding data to None image"
    | Some buf ->
       Str.split (Str.regexp " +") line |>
       List.map (fun s -> Scanf.sscanf s "%x" char_of_int ) |>
       List.iter (Buffer.add_char buf)

  let parse_bitmap_line line b =
    if starts_with line "Pos" then
      { b with pos = parse_bm_pos line ~onerror: (fun () -> b.pos) ~process: (fun c -> Some c)}
    else if starts_with line "Scale" then
      { b with scale = parse_bm_scale line ~onerror: (fun () -> b.scale) ~process: (fun s -> Some s)}
    else if starts_with line "Data" then
      { b with data = Some (Buffer.create 1000) }
    else
      (append_bm_line b.data line; b)

  let parse_line line (lib, c,canevas) =
    match c with
    | DescrContext page_size as context ->
       if (String.compare line "$EndDescr" = 0) then
         (lib, BodyContext, canevas)
       else
         lib, context, (parse_descr_line line page_size canevas)
    | ComponentContext comp ->
       if (String.compare line "$EndComp" = 0) then
         (lib, BodyContext, canevas)
       else
         let comp, canevas = parse_component_line lib line comp canevas in
         (lib, (ComponentContext comp), canevas)
    | BodyContext ->
       let c, canevas = parse_body_line (lib, c,canevas) line
       in lib, c, canevas
    | WireContext l ->
       lib, BodyContext, (parse_wire_line
                            line
                            ~onerror: (fun () -> canevas)
                            ~process: (fun (c1, c2) ->
                              let kolor, width  = match l with
                                | Bus | BusEntry -> Blue, Size 5
                                | Wire | WireEntry -> Brown, Size 2
                                | Line -> Black, Size 2
                              in P.paint_line ~kolor ~width c1 c2 canevas))
    | SheetContext sc ->
       if (String.compare line "$EndSheet" = 0) then
         (lib, BodyContext, canevas)
       else
         let nsc, o = parse_sheet_line line sc canevas in
         lib, SheetContext nsc, o
    | TextContext sc ->
       (match sc with
       | None -> failwith "TextContext without definition!"
       | Some v -> (lib, BodyContext, print_text_line line v canevas))
    | BitmapContext b ->
       if (String.compare line "$EndBitmap" = 0) then
         (lib, BodyContext, plot_bitmap b canevas)
       else
         let nb = parse_bitmap_line line b in
         lib, BitmapContext nb, canevas

  let output_context (_, _, canevas):P.t =
     canevas

  let add_lib line (lib, ctxt, canevas) =
    (CPainter.append_lib line lib) |>
    (fun c -> c, ctxt, canevas)

end
