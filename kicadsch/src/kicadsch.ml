module Defs =  KicadDefs
open Defs
module Sigs = KicadSch_sigs
module NewKicad = NewKicadSch
module Lib_sigs = KicadLib_sigs
open Sigs

module MakeSchPainter (P : Painter) :
  SchPainter with type painterContext := P.t = struct
  module CPainter = Kicadlib.MakePainter (P)
  module EltPainter = SchElementPainter.MakePainter (P)

  type rect = {c: coord; dim: coord}

  type linetype = Wire | Bus | Line | WireEntry | BusEntry

  type component =
    | NoComp
    | Unique of single_reference
    | Multiple of multi_reference list

  type componentContext =
    { component: component
    ; sym: string option
    ; origin: coord option
    ; fields: field list }

  type schParseContext =
    | BodyContext
    | DescrContext of coord
    | WireContext of linetype
    | ComponentContext of componentContext
    | SheetContext of rect option
    | TextContext of label option
    | BitmapContext of bitmapContext

  type schContext =
    {
      lib: KicadLibParserV1.t
    ; c: schParseContext
    ; canevas: EltPainter.t
    ; rev: revision
    ; allow_missing_component: bool
    }

  let initial_context ?allow_missing_component:(allow_missing_component=false) rev =
    {lib=KicadLibParserV1.lib (); c=BodyContext; canevas=EltPainter.create (P.get_context ()); rev; allow_missing_component}


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

  let parse_component_line lib (line : string) (comp : componentContext) allow_missing
      canevas : componentContext * EltPainter.t =
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
                          EltPainter.modify_canevas
                           (CPainter.plot_comp (KicadLibParserV1.get_comp_lib lib) sym m_unitnr origin transfo allow_missing)
                            canevas
                        in
                        let draw = EltPainter.draw_field origin transfo is_multi refs in
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


  (* high level parsing *)

  let parse_sheet_line line context canevas : rect option*EltPainter.t =
    match line.[0] with
    | 'F' ->
        ( context
        , parse_sheet_field line
            ~onerror:(fun () -> canevas)
            ~process:(fun number ->
              if number < 2 then
                parse_sheet_field01 line
                  ~onerror:(fun () -> canevas)
                  ~process:(fun (number, name, s) ->
                    match context with
                      | Some {c; dim} ->
                        EltPainter.draw_sheet_field name number s c dim canevas
                    | None ->
                        canevas )
              else
                parse_sheet_other_fields line
                  ~onerror:(fun () -> canevas)
                  ~process:(fun (name, ptype, justif, c, s) ->
                    EltPainter.draw_port name ptype justif c s canevas ) ) )
    | 'S' ->
        parse_sheet_rect line
          ~onerror:(fun () -> (context, canevas))
          ~process:(fun ({c; dim} as range) ->
            (Some range, EltPainter.draw_sheet_rect c dim canevas) )
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
          ;canevas=EltPainter.draw_page_frame f_left ctx.canevas } )
    else if starts_with line "Wire" || starts_with line "Entry" then
      ( parse_wire_wire line
          ~onerror:(fun () -> {ctx with c=BodyContext})
          ~process:(fun lt ->  {ctx with c=WireContext lt}))
    else if starts_with line "NoConn" then
      {ctx with c=BodyContext
      ; canevas=(parse_noconn_line line
          ~onerror:(fun () -> ctx.canevas)
          ~process:(fun c -> EltPainter.draw_no_connect c ctx.canevas))}
    else if starts_with line "Connection" then
      parse_conn_line line
          ~onerror:(fun () -> ctx)
          ~process:(fun conn_c ->
              {ctx with c=BodyContext
                      ; canevas=EltPainter.draw_junction conn_c ctx.canevas})
    else if String.compare line "$Sheet" = 0 then {ctx with c=SheetContext None}
    else if starts_with line "Text" then
      let lab : label option =
        parse_text_line line
          ~onerror:(fun () -> None)
          ~process:(fun l -> Some l)
      in
      {ctx with c=TextContext lab}
    else {ctx with c=BodyContext}

  let parse_descr_line line c canevas =
    parse_descr_body line
      ~onerror:(fun () -> canevas)
      ~process:(fun (field, content) -> EltPainter.draw_title_field c field content canevas)

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

  let write_revision c ctx =
    EltPainter.write_revision c ctx.rev ctx.canevas

  let parse_line line ctx =
    match ctx.c with
    | DescrContext page_size as c ->
      if String.compare line "$EndDescr" = 0 then
        let canevas = write_revision page_size ctx in
        {ctx with c=BodyContext; canevas}
      else {ctx with c;canevas=(parse_descr_line line page_size ctx.canevas)}
    | ComponentContext comp ->
        if String.compare line "$EndComp" = 0 then {ctx with c=BodyContext}
        else
          let comp, canevas = parse_component_line ctx.lib line comp ctx.allow_missing_component ctx.canevas in
          {ctx with c=ComponentContext comp; canevas}
    | BodyContext ->
        parse_body_line ctx line
    | WireContext l ->
      parse_wire_line line
            ~onerror:(fun () -> {ctx with c=BodyContext})
            ~process:(fun (start, stop) ->
                let canevas =
                  match l with
                  | Bus -> EltPainter.draw_bus [start; stop] false ctx.canevas
                  | BusEntry -> EltPainter.draw_bus [start; stop] true ctx.canevas
                  | Wire -> EltPainter.draw_wire [start; stop] false ctx.canevas
                  | WireEntry -> EltPainter.draw_wire [start; stop] true ctx.canevas
                  | Line -> EltPainter.draw_line [start; stop] ctx.canevas
                in {ctx with c=BodyContext;canevas})
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
          {ctx with c=BodyContext; canevas=EltPainter.draw_text_line line v ctx.canevas} )
    | BitmapContext b ->
        if String.compare line "$EndBitmap" = 0 then
          {ctx with c=BodyContext; canevas=EltPainter.draw_bitmap b ctx.canevas}
        else
          let nb = parse_bitmap_line line b in
          {ctx with c=BitmapContext nb}

  let output_context ({canevas;_ }:schContext) = EltPainter.get_context canevas

  let is_suffix ~suffix s =
    let suff_length = String.length suffix in
    let s_length = String.length s in
    (suff_length < s_length) &&
    (String.equal (String.sub s (String.length s - suff_length) suff_length) suffix)

  let trim_cr l = if is_suffix ~suffix:"\r" l then String.sub l 0 (String.length l - 1)   else l

  let add_lib content ctxt =
    let lines = String.split_on_char  '\n' content in
    let lib = List.fold_left (fun c l -> KicadLibParserV1.append_lib (trim_cr l) c) ctxt.lib lines in
    {ctxt with lib}

  let parse_sheet initctx content =
    let parse c l =
      let trimmed_line = trim_cr l in
      parse_line trimmed_line c in
    let lines = String.split_on_char '\n' content in
    List.fold_left parse initctx lines

end
