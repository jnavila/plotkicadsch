open KicadSch_sigs
open KicadDefs

module type EltPainter =
sig
  type t
  (** A drawing context *)

  type drawContext

  val create: drawContext -> t

  val draw_field : coord -> (int * int) * (int * int) -> bool -> multi_reference list -> t -> field -> t

  val draw_port: ?kolor:[> `Black ] -> string -> porttype -> justify -> coord -> size -> t -> t

  val draw_junction: coord -> t -> t

  val draw_no_connect: coord -> t -> t

  val draw_bitmap: bitmapContext -> t -> t

  val draw_wire: coord list -> bool -> t -> t

  val draw_bus: coord list -> bool -> t  -> t

  val draw_line: coord list -> t -> t

  val draw_title_field: coord -> string -> string -> t -> t

  val draw_text_line: string -> label -> t -> t

  val draw_sheet_field: String.t -> int -> size -> coord -> coord -> t -> t

  val draw_sheet_rect: coord -> coord -> t -> t

  val draw_label: string -> label -> t -> t

  val draw_page_frame: coord -> t -> t

  val write_revision: coord -> revision -> t -> t

  val modify_canevas: (drawContext -> drawContext*bool) -> t -> t*bool

  val get_context : t -> drawContext
  (** [get_context ctx]
      @return the painting canvas attached to the painter [ctx] *)

end

module MakePainter(P: Painter): EltPainter with type drawContext := P.t =
struct
  type wireDesc =
    { start: coord
    ; stop: coord }

  type connectionDesc = coord

  type wires =
    { wires: wireDesc list
    ; cons: connectionDesc list
    ; buses: wireDesc list
    }

  type t =
    { wires: wires
    ; canevas: P.t
    }

  type drawContext = P.t

  let create canevas = {wires={wires=[]; cons=[]; buses=[]}; canevas}

  let swap_justify = function
    | J_left -> J_right
    | J_center -> J_center
    | J_right -> J_left
    | J_bottom -> J_top
    | J_top -> J_bottom

  let orientation_of_justify = function
    | J_left | J_right | J_center -> Orient_H
    | J_top | J_bottom -> Orient_V

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
    {context with canevas=P.paint_text text o' (Coord (x', y')) s j' stl context.canevas}

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
      (Size l as s) ctx =
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
    {ctx with canevas=
    (P.paint_text new_port_name orient c s j NoStyle ctx.canevas)}

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

  let swap_type = function
    | (UnSpcPort | ThreeStatePort | NoPort | BiDiPort) as p ->
        p
    | OutputPort ->
        InputPort
    | InputPort ->
        OutputPort
(*
  let print_text_line line l ctx =
    let c = ctx.canevas in
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
        let canevas = List.fold_left paint_line c (List.mapi (fun i l -> (i, l)) lines) in
        {ctx with canevas}
    | PortLabel (prange, ptype) ->
        let pcolor = match prange with Glabel -> `Green | Hlabel -> `Red in
        let new_type = swap_type ptype in
        draw_port ~kolor:pcolor line new_type l.orient l.c l.size ctx
*)

  let draw_junction conn_c ctx =
    let delta = 10 in
    let canevas = P.paint_circle ~fill:`Black conn_c delta ctx.canevas in
    let wires = {ctx.wires with cons=conn_c::ctx.wires.cons} in
    {canevas; wires}

  let draw_no_connect (Coord (x, y)) ctx =
    let delta = 10 in
    let canevas =
      ctx.canevas
      |> P.paint_line
        (Coord (x - delta, y - delta))
        (Coord (x + delta, y + delta))
      |> P.paint_line
        (Coord (x - delta, y + delta))
        (Coord (x + delta, y - delta)) in
    {ctx with canevas}

  let draw_bitmap b ctx =
    let canevas =     match (b.pos, b.scale, b.data) with
    | Some p, Some s, Some d ->
        P.paint_image p s d ctx.canevas
    | _ ->
      ctx.canevas in
    {ctx with canevas}

  let draw_wire l isEntry ctx =
    if isEntry then
      match l with
      | [start; stop] ->
        {
          canevas=(P.paint_line ~kolor:`Brown ~width:(Size 2) start stop ctx.canevas)
        ; wires={ctx.wires with cons=start::stop::ctx.wires.cons}
        }
      | _ -> raise Not_found     (* TODO: find better exception *)
    else
      match l with
      | start::(_::_ as tail)  ->
      let prev = ref start in
      let append_and_memo wires point =
        let start = !prev in
        (prev := point; {start; stop=point}::wires) in
      let wires = List.fold_left append_and_memo ctx.wires.wires tail in
      {ctx with wires={ctx.wires with wires}}
      | _ -> raise Not_found     (* TODO: find better exception *)

  let draw_bus l isEntry ctx =
    if isEntry then
      match l with
      | [start; stop] ->
        {
          canevas=(P.paint_line ~kolor:`Blue ~width:(Size 5) start stop ctx.canevas)
        ; wires={ctx.wires with cons=start::stop::ctx.wires.cons}
        }
      | _ -> raise Not_found     (* TODO: find better exception *)
    else
      match l with
      | start::(_::_ as tail)  ->
      let prev = ref start in
      let append_and_memo buses point =
        let start = !prev in
        (prev := point; {start; stop=point}::buses) in
      let buses = List.fold_left append_and_memo ctx.wires.buses tail in
      {ctx with wires={ctx.wires with buses}}
      | _ -> raise Not_found     (* TODO: find better exception *)
;;
  let draw_line (l: coord list)  (ctx: t) =
   match l with
      | start::(_::_ as tail)  ->
      let prev = ref start in
      let append_and_memo canevas point =
        let start = !prev in
        (prev := point; P.paint_line ~kolor:`Black ~width:(Size 2) start point canevas) in
      let canevas = List.fold_left append_and_memo ctx.canevas tail in
      {ctx with canevas}
      | _ -> raise Not_found     (* TODO: find better exception *)
;;
  let draw_title_field (Coord (x, y)) field content ctx =
    let cvs = ctx.canevas in
    let canevas =
      if String.length content > 0 then
        let title_text content x y s =
          P.paint_text content Orient_H
            (Coord (x, y))
            (Size s) J_left NoStyle cvs
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
          cvs
      else cvs
    in {ctx with canevas}

  let draw_text_line line l (c: t) =
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
        let canevas = List.fold_left paint_line c.canevas (List.mapi (fun i l -> (i, l)) lines) in
        {c with canevas}
    | PortLabel (prange, ptype) ->
        let pcolor = match prange with Glabel -> `Green | Hlabel -> `Red in
        let new_type = swap_type ptype in
        draw_port ~kolor:pcolor line new_type l.orient l.c l.size c

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

  let get_context ctx : drawContext = cut_wires_and_buses ctx.wires ctx.canevas


  let draw_page_frame (Coord (x, y)) ctx =
    let b_width = 100 in
    let f_width = 4000 in
    let bot_x = x - b_width in
    let bot_y = y - b_width in
    let frame_x = bot_x - f_width in
    let canevas = ctx.canevas
                  |> P.set_canevas_size x y
                  |> P.paint_rect
                    (Coord (b_width, b_width))
                    (Coord (x - (2 * b_width), y - (2 * b_width)))
                  |> P.paint_rect (Coord (frame_x, bot_y - 150)) (Coord (f_width, 150))
                  |> P.paint_rect (Coord (frame_x, bot_y - 250)) (Coord (f_width, 100))
                  |> P.paint_rect (Coord (frame_x, bot_y - 550)) (Coord (f_width, 400))
    in {ctx with canevas}

  let write_revision (Coord (x, y)) rev ctx =
    {ctx with canevas=
    match rev with
    | First s ->
    P.paint_text s Orient_H
              (Coord (x, y + 50))
              (Size 50) J_left NoStyle ctx.canevas
    | Second s ->
    P.paint_text s Orient_H
              (Coord (x + 2200, y + 50))
              (Size 50) J_left NoStyle ctx.canevas
    | No_Rev -> ctx.canevas
    }

  let draw_sheet_field name number ((Size size) as s) (Coord (x, y)) (Coord (_, dim_y)) ctx =
    let y = if number = 0 then y else y + dim_y + size in
    let canevas = P.paint_text name Orient_H
        (Coord (x, y))
        s J_left NoStyle ctx.canevas
    in {ctx with canevas}

  let draw_sheet_rect c dim ctx =
    {ctx with canevas=P.paint_rect c dim ctx.canevas}

  let draw_label line l c : t =
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
        {c with canevas=
        List.fold_left paint_line c.canevas (List.mapi (fun i l -> (i, l)) lines)}
    | PortLabel (prange, ptype) ->
        let pcolor = match prange with Glabel -> `Green | Hlabel -> `Red in
        let new_type = swap_type ptype in
        draw_port ~kolor:pcolor line new_type l.orient l.c l.size c

  let modify_canevas f ctx =
    let canevas, is_multi = f ctx.canevas in
    {ctx with canevas}, is_multi
end
