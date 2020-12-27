module Sigs = KicadSch_sigs
open Sigs

module MakeSchPainter (P : Painter) :
  SchPainter with type painterContext := P.t = struct
  module CPainter = Kicadlib.MakePainter (P)

  type rect = {c: coord; dim: coord}

  type portrange = Glabel | Hlabel

  type labeluse = WireLabel | TextNote

  type porttype =
    | UnSpcPort
    | ThreeStatePort
    | OutputPort
    | InputPort
    | NoPort
    | BiDiPort

  type linetype = Wire | Bus | Line | WireEntry | BusEntry

  type labeltype = PortLabel of portrange * porttype | TextLabel of labeluse

  type label = {c: coord; size: size; orient: justify; labeltype: labeltype}

  type field =
    { nb: int
    ; text: string
    ; o: orientation
    ; co: coord
    ; s: size
    ; j: justify
    ; stl: style }

  type single_reference = {piece: string option; unitnr: int option}

  type multi_reference = {m_piece: string; m_unitnr: int}

  type component =
    | NoComp
    | Unique of single_reference
    | Multiple of multi_reference list

  type componentContext =
    { component: component
    ; sym: string option
    ; origin: coord option
    ; fields: field list }

  type bitmapContext =
    {pos: coord option; scale: float option; data: Buffer.t option}

  type schParseContext =
    | BodyContext
    | DescrContext of coord
    | WireContext of linetype
    | ComponentContext of componentContext
    | SheetContext of rect option
    | TextContext of label option
    | BitmapContext of bitmapContext

  type wireDesc =
    { start: coord
    ; stop: coord }

  type connectionDesc = coord
  type wires =
    { wires: wireDesc list
    ; cons: connectionDesc list
    ; buses: wireDesc list
    }

  type schContext =
    { wires: wires
    ; lib: CPainter.t
    ; c: schParseContext
    ; canevas: P.t
    }
  type ('a, 'b) either =
      Left of 'a | Right of 'b

  let initial_context () = {wires={wires=[]; cons=[]; buses=[]}; lib=CPainter.lib (); c=BodyContext; canevas=P.get_context ()}

  let swap_type = function
    | (UnSpcPort | ThreeStatePort | NoPort | BiDiPort) as p ->
        p
    | OutputPort ->
        InputPort
    | InputPort ->
        OutputPort

  let porttype_of_string = function
    | "U" | "UnSpc" ->
        UnSpcPort
    | "T" | "3State" ->
        ThreeStatePort
    | "O" | "Output" ->
        OutputPort
    | "I" | "Input" ->
        InputPort
    | "B" | "BiDi" ->
        BiDiPort
    | "~" ->
        NoPort
    | _ as s ->
        ignore (Printf.printf "unknown port type %s\n" s) ;
        NoPort

  let justify_of_string s =
    match s.[0] with
    | 'L' | '0' ->
        J_left
    | 'R' | '2' ->
        J_right
    | 'C' ->
        J_center
    | 'B' | '1' ->
        J_bottom
    | 'T' | '3' ->
        J_top
    | c ->
        failwith (Printf.sprintf "no match for justify! (%c)" c)

  let style_of_string s =
    let i = (fst s).[0] and b = (snd s).[0] in
    match (i, b) with
    | 'N', 'B' ->
        Bold
    | 'I', 'N' ->
        Italic
    | 'I', 'B' ->
        BoldItalic
    | 'N', 'N' ->
        NoStyle
    | _ ->
        failwith (Printf.sprintf "no match for style! (%c %c)" i b)

  let orientation_of_string s =
    match s.[0] with
    | 'H' ->
        Orient_H
    | 'V' ->
        Orient_V
    | c ->
        failwith (Printf.sprintf "no match for orientation! (%c)" c)

  let orientation_of_justify = function
    | J_left | J_right | J_center ->
        Orient_H
    | J_top | J_bottom ->
        Orient_V

  (* Parsing a sch file *)
  open Schparse

  let parse_F =
    create_parse_fun ~name:"Component F"
      ~regexp_str:"F %d %S %[HV] %d %d %d %[01] %[LRCBT] %[CLRBTNI]"
      ~extract_fun:(fun nb name orient posX posY size flags hjust vjustbi ->
        let co = Coord (posX, posY)
        and o = orientation_of_string orient
        and s = Size size
        and j = justify_of_string hjust
        and stl =
          style_of_string (String.sub vjustbi 1 1, String.sub vjustbi 2 1)
        and visible = flags.[3] = '0' && not (String.equal "~" name) in
        Some (nb, visible, name, o, co, s, j, stl) )

  let parse_L =
    create_parse_fun ~name:"Component L" ~regexp_str:"L %s %s"
      ~extract_fun:(fun name reference -> Some (name, reference))

  let parse_P =
    create_parse_fun ~name:"Component P" ~regexp_str:"P %d %d"
      ~extract_fun:(fun x y -> Some (Coord (x, y)))

  let parse_U =
    create_parse_fun ~name:"Component U" ~regexp_str:"U %d %s %s"
      ~extract_fun:(fun n mm timestamp -> Some (n, mm, timestamp))

  let parse_AR =
    create_parse_fun ~name:"Component AR" ~regexp_str:"AR %s %s %s"
      ~extract_fun:(fun _ ref_s part_s ->
        let the_ref = String.sub ref_s 5 (String.length ref_s - 6) in
        let the_part =
          int_of_string @@ String.sub part_s 6 (String.length part_s - 7)
        in
        Some (the_ref, the_part) )

  let parse_transfo =
    let check x = x = 1 || x = 0 || x = -1 in
    create_parse_fun ~name:"Component transformation"
      ~regexp_str:" %d %d %d %s" ~extract_fun:(fun a b c ds ->
        if String.length ds > 0 then
          let d = int_of_string ds in
          if check a && check b && check c && check d then
            Some (a, b, c, Some d)
          else (
            Printf.printf "Bad transfo matrix! %d %d %d %d\n" a b c d ;
            None )
        else Some (a, b, c, None) )

  let swap_justify = function
    | J_left ->
        J_right
    | J_center ->
        J_center
    | J_right ->
        J_left
    | J_bottom ->
        J_top
    | J_top ->
        J_bottom

  let draw_field (Coord (x0, y0)) ((a, b), (c, d)) is_multi refs context
      {nb; text; o; co; s; j; stl} =
    let (Coord (x, y)) = co in
    let xrel = x - x0 and yrel = y - y0 in
    let x' = (a * xrel) + (b * yrel) + x0 in
    let y' = (c * xrel) + (d * yrel) + y0 in
    let o' =
      if a = 0 then
        (* this is a ±90° rotation matrix *)
        match o with Orient_H -> Orient_V | Orient_V -> Orient_H
      else o
    in
    let text =
      if nb != 0 then text
      else
        String.concat "/"
          (List.map
             (fun {m_unitnr; m_piece} ->
               if is_multi then
                 m_piece
                 ^ Char.escaped (char_of_int (m_unitnr + int_of_char 'A' - 1))
               else m_piece )
             refs)
    in
    let j' =
      match o' with
      | Orient_H ->
          if a = -1 || b = -1 then swap_justify j else j
      | Orient_V ->
          if c = 1 || d = -1 then swap_justify j else j
    in
    P.paint_text text o' (Coord (x', y')) s j' stl context

  let right_arrow = "\xE2\x96\xB6"

  let left_arrow = "\xE2\x97\x80"

  let diamond = "\xE2\x97\x86"

  let square = "\xE2\x97\xBC"

  let decorate_port_name name ptype justif =
    let port_char =
      match (ptype, justif) with
      | UnSpcPort, _ | NoPort, _ ->
          ""
      | ThreeStatePort, _ | BiDiPort, _ ->
          diamond
      | OutputPort, (J_left | J_top) | InputPort, (J_right | J_bottom) ->
          left_arrow
      | OutputPort, (J_right | J_bottom) | InputPort, (J_left | J_top) ->
          right_arrow
      | _, J_center ->
          square
    in
    match justif with
    | J_left | J_top ->
        port_char ^ name
    | J_right | J_bottom ->
        name ^ port_char
    | J_center ->
        name

  let draw_port ?(kolor = `Black) name ptype justif (Coord (x, y))
      (Size l as s) canevas =
    let new_port_name = decorate_port_name name ptype justif in
    let orient = orientation_of_justify justif in
    let j = justif in
    let _ = kolor in
    let c =
      match orient with
      | Orient_H ->
          Coord (x, y + (l / 4))
      | Orient_V ->
          Coord (x + (l / 4), y)
    in
    P.paint_text new_port_name orient c s j NoStyle canevas

  let parse_component_line lib (line : string) (comp : componentContext)
      canevas : componentContext * P.t =
    let update_comp comp = (comp, canevas) in
    if String.length line == 0 then
      comp, canevas
    else
    let first = line.[0] in
    match first with
    | 'A' ->
        update_comp
        @@ parse_AR line
             ~onerror:(fun () -> comp)
             ~process:(fun (the_ref, the_unit) ->
               if the_ref.[String.length the_ref - 1] = '?' then comp
               else
                 let new_name = {m_piece= the_ref; m_unitnr= the_unit} in
                 let component =
                   Multiple
                     ( match comp.component with
                     | NoComp | Unique _ ->
                         [new_name]
                     | Multiple l ->
                         new_name :: l )
                 in
                 {comp with component} )
    | 'F' ->
        update_comp
        @@ parse_F line
             ~onerror:(fun () -> comp)
             ~process:(fun (nb, visible, text, o, co, s, j, stl) ->
               if visible && String.length text > 0 then
                 {comp with fields= {nb; text; o; co; s; j; stl} :: comp.fields}
               else comp )
    | 'U' ->
        update_comp
        @@ parse_U line
             ~onerror:(fun () -> comp)
             ~process:(fun (u, _, _) ->
               let component =
                 match comp.component with
                 | NoComp ->
                     Unique {piece= None; unitnr= Some u}
                 | Unique r ->
                     Unique {r with unitnr= Some u}
                 | Multiple _ ->
                     comp.component
               in
               {comp with component} )
    | 'P' ->
        update_comp
        @@ parse_P line
             ~onerror:(fun () -> comp)
             ~process:(fun c -> {comp with origin= Some c})
    | 'L' ->
        update_comp
        @@ parse_L line
             ~onerror:(fun () -> comp)
             ~process:(fun (sym_s, n) ->
               let component =
                 match comp.component with
                 | NoComp ->
                     Unique {piece= Some n; unitnr= None}
                 | Unique r ->
                     Unique {r with piece= Some n}
                 | Multiple _ ->
                     comp.component
               in
               let sym = Some sym_s in
               {comp with component; sym} )
    | '	' ->
        parse_transfo line
          ~onerror:(fun () -> (comp, canevas))
          ~process:(fun (a, b, c, d_opt) ->
            match d_opt with
            | Some d -> (
                let {component; origin; fields; sym} = comp in
                match (origin, sym) with
                | Some origin, Some sym -> (
                    let res =
                      match component with
                      | Unique {unitnr= Some m_unitnr; piece= Some m_piece} ->
                          Some ([{m_unitnr; m_piece}], m_unitnr)
                      | Multiple m -> (
                        match m with
                        | [] ->
                            None
                        | c :: _ ->
                            Some (m, c.m_unitnr) )
                      | Unique {unitnr= None; _}
                      | Unique {piece= None; _}
                      | NoComp ->
                          None
                    in
                    match res with
                    | None ->
                        Printf.printf
                          "cannot plot component with missing definitions !" ;
                        (comp, canevas)
                    | Some (refs, m_unitnr) ->
                        let transfo = ((a, b), (c, d)) in
                        let canevas', is_multi =
                          CPainter.plot_comp lib sym m_unitnr origin transfo
                            canevas
                        in
                        let draw = draw_field origin transfo is_multi refs in
                        (comp, List.fold_left draw canevas' fields) )
                | _ ->
                    Printf.printf
                      "cannot plot component with missing definitions !" ;
                    (comp, canevas) )
            | None ->
                (comp, canevas) )
    | _ ->
        ignore (Printf.printf "ignored %s\n" line) ;
        (comp, canevas)

  let parse_wire_wire =
    create_parse_fun ~name:"Wire header" ~regexp_str:"%s %s %s"
      ~extract_fun:(fun kind width line ->
        match (kind, width, line) with
        | "Wire", "Wire", "Line" ->
            Some Wire
        | "Wire", "Bus", "Line" ->
            Some Bus
        | "Wire", "Notes", "Line" ->
            Some Line
        | "Wire", "Wire", "Note" ->
            Some Line
        | "Entry", "Wire", "Line" ->
            Some WireEntry
        | "Entry", "Bus", "Line" ->
            Some BusEntry
        | _, _, _ ->
            None )

  let parse_wire_line =
    create_parse_fun ~name:"Wire" ~regexp_str:" %d %d %d %d"
      ~extract_fun:(fun x1 y1 x2 y2 ->
        let c1 = Coord (x1, y1) and c2 = Coord (x2, y2) in
        Some (c1, c2) )

  let parse_noconn_line =
    create_parse_fun ~name:"NoConn" ~regexp_str:"NoConn ~ %d %d"
      ~extract_fun:(fun x y -> Some (Coord (x, y)))

  let parse_conn_line =
    create_parse_fun ~name:"Connection" ~regexp_str:"Connection ~ %d %d"
      ~extract_fun:(fun x y -> Some (Coord (x, y)))

  let parse_sheet_field01 =
    create_parse_fun ~name:"Sheet Field 0 or 1" ~regexp_str:"F%[01] %S %d"
      ~extract_fun:(fun num name size ->
        let number = int_of_string num in
        Some (number, name, Size size) )

  let parse_sheet_other_fields =
    create_parse_fun ~name:"Sheet generic field"
      ~regexp_str:"F%d %S  %[IOBTU] %[RLTB] %d %d %d"
      ~extract_fun:(fun _ name t j x y sz ->
        let ptype = porttype_of_string t in
        let justif = justify_of_string j in
        let c = Coord (x, y) in
        let s = Size sz in
        Some (name, ptype, justif, c, s) )

  let parse_sheet_field =
    create_parse_fun ~name:"detect sheet field" ~regexp_str:"F%d"
      ~extract_fun:(fun num -> Some num)

  let parse_sheet_rect =
    create_parse_fun ~name:"Sheet Rect" ~regexp_str:"S %d %d %d %d"
      ~extract_fun:(fun x1 y1 x2 y2 ->
        let c = Coord (x1, y1) and dim = Coord (x2, y2) in
        Some {c; dim} )

  let parse_text_line =
    create_parse_fun ~name:"Text header" ~regexp_str:"Text %s %d %d %s %d %s"
      ~extract_fun:(fun ltype x y j s lorient ->
        let c = Coord (x, y) and j = justify_of_string j and size = Size s in
        let labeltype, orient =
          match ltype with
          | "GLabel" ->
              (PortLabel (Glabel, porttype_of_string lorient), swap_justify j)
          | "HLabel" ->
              (PortLabel (Hlabel, porttype_of_string lorient), swap_justify j)
          | "Label" ->
              (TextLabel WireLabel, j)
          | "Notes" ->
              (TextLabel TextNote, j)
          | _ ->
              (TextLabel TextNote, j)
        in
        let result : label option = Some {size; orient; labeltype; c} in
        result )

  let parse_descr_header =
    create_parse_fun ~name:"Descr header" ~regexp_str:"$Descr %s %d %d"
      ~extract_fun:(fun format x y -> Some (format, Coord (x, y)))

  let parse_descr_body =
    create_parse_fun ~name:"Description line" ~regexp_str:"%s %s@^"
      ~extract_fun:(fun field value ->
        if value.[0] = '"' then
          let new_val = String.sub value 1 (String.length value - 2) in
          Some (field, new_val)
        else Some (field, value) )

  let parse_bm_pos =
    create_parse_fun ~name:"Bitmap Pos" ~regexp_str:"Pos %d %d"
      ~extract_fun:(fun x y -> Some (Coord (x, y)))

  let parse_bm_scale =
    create_parse_fun ~name:"Bitmap Scale" ~regexp_str:"Scale %f"
      ~extract_fun:(fun sc -> Some sc)

  (* Printing things *)

  let split_lines line =
    let len = String.length line in
    let rec split lstart lend (acc : string list) =
      if lend < len - 1 then
        if line.[lend] = '\\' && line.[lend + 1] = 'n' then
          split (lend + 2) (lend + 2)
            (String.sub line lstart (lend - lstart) :: acc)
        else split lstart (lend + 1) acc
      else String.sub line lstart (len - lstart) :: acc
    in
    split 0 0 []

  let print_text_line line l c =
    match l.labeltype with
    | TextLabel t ->
        let pcolor = match t with TextNote -> `Green | WireLabel -> `Red in
        let (Size s) = l.size in
        let (Coord (x, y)) = l.c in
        let paint_line c' (line_index, l') =
          P.paint_text ~kolor:pcolor l'
            (orientation_of_justify l.orient)
            (Coord (x, y - (line_index * s)))
            l.size l.orient NoStyle c'
        in
        let lines = split_lines line in
        List.fold_left paint_line c (List.mapi (fun i l -> (i, l)) lines)
    | PortLabel (prange, ptype) ->
        let pcolor = match prange with Glabel -> `Green | Hlabel -> `Red in
        let new_type = swap_type ptype in
        draw_port ~kolor:pcolor line new_type l.orient l.c l.size c

  let plot_page_frame (Coord (x, y)) canevas =
    let b_width = 100 in
    let f_width = 4000 in
    let bot_x = x - b_width in
    let bot_y = y - b_width in
    let frame_x = bot_x - f_width in
    canevas
    |> P.paint_rect
         (Coord (b_width, b_width))
         (Coord (x - (2 * b_width), y - (2 * b_width)))
    |> P.paint_rect (Coord (frame_x, bot_y - 150)) (Coord (f_width, 150))
    |> P.paint_rect (Coord (frame_x, bot_y - 250)) (Coord (f_width, 100))
    |> P.paint_rect (Coord (frame_x, bot_y - 550)) (Coord (f_width, 400))

  let plot_bitmap b context =
    match (b.pos, b.scale, b.data) with
    | Some p, Some s, Some d ->
        P.paint_image p s d context
    | _ ->
        context

  (* high level parsing *)

  let parse_sheet_line line context canevas =
    match line.[0] with
    | 'F' ->
        ( context
        , parse_sheet_field line
            ~onerror:(fun () -> canevas)
            ~process:(fun number ->
              if number < 2 then
                parse_sheet_field01 line
                  ~onerror:(fun () -> canevas)
                  ~process:(fun (number, name, (Size size as s)) ->
                    match context with
                    | Some {c= Coord (x, y); dim= Coord (_, dim_y)} ->
                        let y = if number = 0 then y else y + dim_y + size in
                        P.paint_text name Orient_H
                          (Coord (x, y))
                          s J_left NoStyle canevas
                    | None ->
                        canevas )
              else
                parse_sheet_other_fields line
                  ~onerror:(fun () -> canevas)
                  ~process:(fun (name, ptype, justif, c, s) ->
                    draw_port name ptype justif c s canevas ) ) )
    | 'S' ->
        parse_sheet_rect line
          ~onerror:(fun () -> (context, canevas))
          ~process:(fun ({c; dim} as range) ->
            (Some range, P.paint_rect c dim canevas) )
    | 'U' ->
        (context, canevas)
    | _ ->
        Printf.printf "unknown sheet line (%s)" line ;
        (context, canevas)

  let starts_with str p =
    let len = String.length p in
    if String.length str < len then false
    else
      let rec comp_rec str p i =
        if str.[i] <> p.[i] then false
        else if i = len - 1 then true
        else comp_rec str p (i + 1)
      in
      comp_rec str p 0

  let parse_body_line ctx line =
    if String.compare line "$Comp" = 0 then
      {ctx with c=ComponentContext
          {component= NoComp; sym= None; origin= None; fields= []}}

    else if String.compare line "$Bitmap" = 0 then
      {ctx with c=BitmapContext {pos= None; scale= None; data= None}}
    else if starts_with line "$Descr" then
      parse_descr_header line
        ~onerror:(fun () -> {ctx with c=BodyContext})
        ~process:(fun (_, (Coord (x, y) as f_left)) ->
          {ctx with c=DescrContext (Coord (x - 4000, y - 100))
          ;canevas=plot_page_frame f_left (P.set_canevas_size x y ctx.canevas) } )
    else if starts_with line "Wire" || starts_with line "Entry" then
      ( parse_wire_wire line
          ~onerror:(fun () -> {ctx with c=BodyContext})
          ~process:(fun lt ->  {ctx with c=WireContext lt}))
    else if starts_with line "NoConn" then
      {ctx with c=BodyContext
      ; canevas=(parse_noconn_line line
          ~onerror:(fun () -> ctx.canevas)
          ~process:(fun (Coord (x, y)) ->
            let delta = 20 in
            ctx.canevas
            |> P.paint_line
                 (Coord (x - delta, y - delta))
                 (Coord (x + delta, y + delta))
            |> P.paint_line
                 (Coord (x - delta, y + delta))
                 (Coord (x + delta, y - delta)) ) ) }
    else if starts_with line "Connection" then
      parse_conn_line line
          ~onerror:(fun () -> ctx)
          ~process:(fun conn_c -> {ctx with c=BodyContext
                                          ; canevas=(
          let delta = 10 in
            P.paint_circle ~fill:`Black conn_c delta ctx.canevas)
          ;wires={ctx.wires with cons=conn_c::ctx.wires.cons}} )
    else if String.compare line "$Sheet" = 0 then {ctx with c=SheetContext None}
    else if starts_with line "Text" then
      let lab : label option =
        parse_text_line line
          ~onerror:(fun () -> None)
          ~process:(fun l -> Some l)
      in
      {ctx with c=TextContext lab}
    else {ctx with c=BodyContext}

  let parse_descr_line line (Coord (x, y)) canevas =
    parse_descr_body line
      ~onerror:(fun () -> canevas)
      ~process:(fun (field, content) ->
        if String.length content > 0 then
          let title_text content x y s =
            P.paint_text content Orient_H
              (Coord (x, y))
              (Size s) J_left NoStyle canevas
          in
          match field with
          | "Sheet" ->
              title_text ("Page: " ^ content) x (y - 200) 50
          | "Title" ->
              title_text ("Title: " ^ content) x (y - 50) 100
          | "Rev" ->
              title_text ("Rev: " ^ content) (x + 3200) (y - 50) 100
          | "Date" ->
              title_text ("Date: " ^ content) (x + 500) (y - 200) 50
          | "Comp" ->
              title_text content (x + 1000) (y - 200) 50
          | "Comment1" ->
              title_text content x (y - 400) 50
          | "Comment2" ->
              title_text content (x + 2000) (y - 400) 50
          | "Comment3" ->
              title_text content x (y - 300) 50
          | "Comment4" ->
              title_text content (x + 2000) (y - 300) 50
          | _ ->
              canevas
        else canevas )

  let append_bm_line data_opt line =
    match data_opt with
    | None ->
        failwith "not adding data to None image"
    | Some buf ->
        parse_list " %x " line |> List.rev_map char_of_int
        |> List.iter (Buffer.add_char buf)

  let parse_bitmap_line line b =
    if starts_with line "Pos" then
      { b with
        pos=
          parse_bm_pos line
            ~onerror:(fun () -> b.pos)
            ~process:(fun c -> Some c) }
    else if starts_with line "Scale" then
      { b with
        scale=
          parse_bm_scale line
            ~onerror:(fun () -> b.scale)
            ~process:(fun s -> Some s) }
    else if starts_with line "Data" then
      {b with data= Some (Buffer.create 1000)}
    else ( append_bm_line b.data line ; b )

  let parse_line line (ctx:schContext) =
    match ctx.c with
    | DescrContext page_size as c ->
        if String.compare line "$EndDescr" = 0 then {ctx with c=BodyContext}
        else {ctx with c;canevas=(parse_descr_line line page_size ctx.canevas)}
    | ComponentContext comp ->
        if String.compare line "$EndComp" = 0 then {ctx with c=BodyContext}
        else
          let comp, canevas = parse_component_line ctx.lib line comp ctx.canevas in
          {ctx with c=ComponentContext comp; canevas}
    | BodyContext ->
        parse_body_line ctx line
    | WireContext l ->
      parse_wire_line line
            ~onerror:(fun () -> {ctx with c=BodyContext})
            ~process:(fun (start, stop) ->
                let paint_param =
                  match l with
                  | Bus -> Right true
                  | BusEntry ->
                    Left ((`Blue, Size 5), true)
                  | Wire -> Right false
                  | WireEntry ->
                    Left ((`Brown, Size 2), true)
                  | Line ->
                    Left ((`Black, Size 2), false)
                in
                begin
                  match paint_param with
                  | Left ((kolor, width), isEntry) ->
                    if isEntry then
                      {ctx with
                       c=BodyContext;canevas=P.paint_line ~kolor ~width start stop ctx.canevas
                       ; wires={ctx.wires with cons=start::stop::ctx.wires.cons}}
                    else
                      {ctx with c=BodyContext;canevas=P.paint_line ~kolor ~width start stop ctx.canevas}
                  | Right isBus ->
                    if isBus then
                      {ctx with c=BodyContext; wires={ctx.wires with buses={start; stop}::ctx.wires.buses}}
                    else
                      {ctx with c=BodyContext; wires={ctx.wires with wires={start; stop}::ctx.wires.wires}}
                end)
    | SheetContext sc ->
        if String.compare line "$EndSheet" = 0 then {ctx with c=BodyContext}
        else
          let nsc, canevas = parse_sheet_line line sc ctx.canevas in
          {ctx with c=SheetContext nsc; canevas}
    | TextContext sc -> (
      match sc with
      | None ->
          failwith "TextContext without definition!"
      | Some v ->
          {ctx with c=BodyContext; canevas= print_text_line line v ctx.canevas} )
    | BitmapContext b ->
        if String.compare line "$EndBitmap" = 0 then
          {ctx with c=BodyContext; canevas=plot_bitmap b ctx.canevas}
        else
          let nb = parse_bitmap_line line b in
          {ctx with c=BitmapContext nb}
  module type OrderedCoord =
  sig
    val compare: coord -> coord -> int
  end

  module SegmentCutter(O:OrderedCoord):(sig val cut_wires: wireDesc list -> coord list -> kolor:kolor -> width:size -> P.t -> P.t end) =
  struct
    module SegmentSet = Set.Make(struct
        type t = wireDesc
        let compare {start=start1; _}  {start=start2; _} = O.compare start1 start2
      end)

    let point_in_segment c {start; stop} =
      (O.compare start c <= 0) && (O.compare stop c >= 0)

    let con_in_a_segment c set =
      match SegmentSet.find_first_opt (fun {stop; _} -> (O.compare stop c > 0)) set with
      | None -> None
      | Some ({start; _} as seg) ->
        if O.compare start c < 0 then
          Some seg
        else
          None
    ;;

    let point_in_a_segment c set =
      match SegmentSet.find_first_opt (fun {stop; _} -> (O.compare stop c >= 0)) set with
      | None -> None
      | Some ({start; _} as seg) ->
        if O.compare start c <= 0 then
          Some seg
        else
          None
    ;;

    let cut_wire set con =
      match con_in_a_segment con set with
      | None -> set
      | Some ({start; stop} as seg) ->
          set |> SegmentSet.remove seg |> SegmentSet.add {start; stop=con} |> SegmentSet.add {start=con;stop}
    ;;

    let merge_segment ~set seg =
      SegmentSet.filter (fun {start=stt; _} -> not( point_in_segment stt seg)) set
      |>SegmentSet.add seg
    ;;

    let insert_segment set {start; stop} =
      let start, stop = if O.compare start stop <= 0 then
          start, stop
        else
          stop, start in
      match (point_in_a_segment start set), (point_in_a_segment stop set) with
      | None, None ->
        merge_segment ~set {start; stop}
      | Some ({start=stt; _}), None ->
        merge_segment ~set {start=stt; stop}
      | None, Some {stop=stp; _} ->
        merge_segment ~set {start; stop=stp}
      | Some {start=stt; _}, Some {stop=stp; _} ->
        merge_segment ~set {start=stt; stop=stp}
    ;;

    let cut_wires seg_list junctions ~kolor ~width canevas =
      let seg_set = List.fold_left insert_segment SegmentSet.empty seg_list in
      let split_set = List.fold_left cut_wire seg_set junctions in
      SegmentSet.fold (fun {start; stop} canevas -> P.paint_line ~kolor ~width start stop canevas) split_set canevas
    ;;
  end

  module VerticalSet = SegmentCutter(
    struct
      let compare (Coord (xs0, ys0)) (Coord (xs1, ys1)) =
      match Stdlib.compare xs0 xs1 with
        | 0 -> Stdlib.compare ys0 ys1
        | c -> c
    end)

  module HorizontalSet = SegmentCutter(
    struct
      let compare (Coord (xs0, ys0)) (Coord (xs1, ys1)) =
      match Stdlib.compare ys0 ys1 with
        | 0 -> Stdlib.compare xs0 xs1
        | c -> c
    end)

  let cut_all_wires junctions wires ~kolor ~width canevas =
    let vertical, horizontal = List.partition (fun {start=Coord (x1, _); stop=Coord (x2, _)} -> x1 == x2) wires in
    VerticalSet.cut_wires vertical junctions ~kolor ~width canevas |>
    HorizontalSet.cut_wires horizontal junctions ~kolor ~width

  let cut_wires_and_buses {wires;buses;cons} canevas =
    cut_all_wires cons wires ~kolor:`Brown ~width:(Size 2) canevas |>
    cut_all_wires cons buses ~kolor:`Blue  ~width:(Size 5)

  let output_context ({canevas; wires;_ }:schContext) = cut_wires_and_buses wires canevas

  let add_lib line ctxt =
    CPainter.append_lib line ctxt.lib |> fun lib -> {ctxt with lib}
end
