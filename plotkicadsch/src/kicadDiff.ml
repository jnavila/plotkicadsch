open Core_kernel
open Lwt.Infix
module S = Kicadsch.MakeSchPainter (SvgPainter)
open Kicadsch.Sigs
include DiffFs

let doc = function
  | TrueFS s -> "file system " ^ s
  | GitFS s -> "Git rev " ^ s

let git_fs commitish = GitFS commitish

type differ = Internal of string | Image_Diff

let true_fs rootname = TrueFS rootname

let fs_mod = function
  | GitFS r -> GitFs.make r
  | TrueFS r -> TrueFs.make r

module L = Kicadsch.MakeSchPainter(ListPainter.L)

module LP = struct
  include L

  type painterContext = ListPainter.listcanevas
end

module FSPainter (S : SchPainter) (F : Simple_FS) : sig
  val find_schematics : unit -> (string list * string) list Lwt.t

  val process_file : S.schContext Lwt.t -> string list -> S.painterContext Lwt.t

  val context_from : S.schContext Lwt.t -> S.schContext Lwt.t
end = struct
  let find_schematics () = F.list_files (String.is_suffix ~suffix:".sch")

  let process_file initctx filename =
    let parse c l = S.parse_line l c in
    let%lwt init = initctx in
    F.get_content filename
    >|= fun ctt ->
    let lines = String.split_lines ctt in
    let endctx = List.fold_left ~f:parse ~init lines in
    S.output_context endctx

  let find_libs () =
    F.list_files (String.is_suffix ~suffix:"-cache.lib") >|= List.map ~f:fst

  let read_libs initial_ctx lib_list =
    Lwt_list.fold_left_s
      (fun c l ->
         F.get_content l
         >|= String.split_lines
         >|= List.fold_left ~f:(fun ctxt l -> S.add_lib l ctxt) ~init:c )
      initial_ctx lib_list

  let context_from from_ctx =
    let%lwt initial_context = from_ctx in
    find_libs () >>= read_libs initial_context
end

let intersect_lists l1l l2l =
  l1l
  >>= fun l1 ->
  l2l
  >|= fun l2 ->
  List.filter
    ~f:(fun (name2, sha2) ->
        List.exists
          ~f:(fun (name1, sha1) ->
              List.equal String.equal name1 name2 && not (String.equal sha2 sha1) )
          l1 )
    l2
  |> List.map ~f:fst

let to_unit _ = ()

module type Differ = sig
  val doc : string

  type pctx

  module S : SchPainter with type painterContext = pctx

  val display_diff :
    from_ctx:pctx -> to_ctx:pctx -> string list -> keep:bool -> bool Lwt.t
end

let internal_diff (d : string) (c : SvgPainter.diff_colors option) (z: string option) =
  ( module struct
    let doc = "internal diff and show with " ^ d

    type pctx = ListPainter.listcanevas

    module S = LP

    type diff_style = Theirs | Ours | Idem

    let plot_elt style out_ctx (arg : ListPainter.t) =
      let open ListPainter in
      let module O = SvgPainter in
      let kolor =
        match style with
        | Theirs ->
          `Old
        | Ours ->
          `New
        | Idem ->
          `ForeGround
      in
      match arg with
      | Text (_, text, o, c, s, j, style) ->
        O.paint_text ~kolor text o c s j style out_ctx
      | Line (_, s, from_, to_) ->
        O.paint_line ~kolor ~width:s from_ to_ out_ctx
      | Rect (_, _, c1, c2) ->
        O.paint_rect ~kolor c1 c2 out_ctx
      | Circle (_, _, center, radius) ->
        O.paint_circle ~kolor center radius out_ctx
      | Arc (_, _, center, start_, end_, radius) ->
        O.paint_arc ~kolor center start_ end_ radius out_ctx
      | Image (corner, scale, data) ->
        O.paint_image corner scale data out_ctx
      | Format (Coord (x, y)) ->
        O.set_canevas_size x y out_ctx
      | Zone (c1, c2) ->
        O.paint_zone c1 c2 out_ctx

    let text_bbox text o c s j =
      (* TODO: vertical text does not work *)
      let len = String.length text in
      let Size sz = s in
      let Coord (x,y) = c in
      let shift =
        match j with
        | J_right | J_bottom -> -sz*len/2
        | J_center -> - sz*len/4
        | J_left | J_top -> 0
      in
      match o with
      | Orient_H ->
        BoundingBox.create_from_rect (Coord (x+shift,y)) (Coord (sz*len/2,sz/2))
      | Orient_V ->
        BoundingBox.create_from_rect (Coord (x, y-sz*len/2+shift)) (Coord (sz/2, sz*len/2))

    let elt_rect elt =
      let open ListPainter in
      let module BB = BoundingBox in
      match elt with
      | Text (_, text, o, c, s, j, _) ->
        text_bbox text o c s j
      | Line (_, _, f, t) ->
        BB.create_from_limits f t
      | Rect (_,  _, c1, c2)
      | Zone (c1, c2) ->
        BB.create_from_rect c1 c2
      | Circle (_, _, center, radius) ->
        let Coord(x,y) = center in
        BB.create_from_limits (Coord(x-radius, y-radius)) (Coord(x+radius,y+radius))
      | Arc (_ , _, center, _, _, radius) ->
        (* TODO: take into count partial angle *)
        let Coord(x,y) = center in
        BB.create_from_limits (Coord(x-radius, y-radius)) (Coord(x+radius,y+radius))
      | Image (corner, _, data) ->
        let w,h = SvgPainter.get_png_dims data in
        BB.create_from_rect corner (Coord(w, h))
      | Format _ -> BB.create ()

    let  dispatch_rect (res, acc) elt =
      let open Float in
      if (BoundingBox.overlap_ratio res elt) > 0.9 then
        BoundingBox.add_rect res elt , acc
      else
          res, elt::acc

    let rec aggregate rect rect_list =
      let result, remaining = List.fold  ~f:dispatch_rect ~init:(rect, []) rect_list in
      if Int.equal (List.length remaining) (List.length rect_list) then
        result, remaining
      else
        aggregate result remaining

    let merge_rects rects:BoundingBox.t list =
      let rec aggregate_list out_list = function
        | rect::l ->
          let res, remaining = aggregate rect l in
          let res2, remaining2 = aggregate res out_list in
          aggregate_list (res2::remaining2) remaining
        | [] -> out_list , [] in
      fst (aggregate_list [] rects)

    let draw_bb ctx r =
      let c1, c2 = BoundingBox.as_rect r  in
      SvgPainter.paint_zone c1 c2 ctx

    let compare s1 s2 =
      let s1_r = elt_rect s1
      and s2_r = elt_rect s2 in
      BoundingBox.compare s1_r s2_r

    let draw_difftotal ~prev ~next out_canevas =
    let rec rec_draw_difftotal ~prev ~next (idem, theirs, ours, outc) diff_list =
      let r s = BoundingBox.reformat ~min_size:20 ~extend:50 (elt_rect s) in
      match prev, next with
      | p::pl, n::nl ->
        let comp = compare p n in
        if comp == 0 then
          rec_draw_difftotal ~prev:pl ~next:nl ((plot_elt Idem idem p),theirs, ours, outc) diff_list
        else if comp < 0 then
          rec_draw_difftotal ~prev:pl ~next (idem, (plot_elt Theirs theirs p), ours, outc) ((r p)::diff_list)
        else
          rec_draw_difftotal ~prev ~next:nl (idem, theirs, (plot_elt Ours ours n), outc) (r n::diff_list)
      | p::pl, [] ->
        rec_draw_difftotal ~prev:pl ~next (idem, (plot_elt Theirs theirs p), ours, outc) (r p::diff_list)
      | [], n::nl ->
        rec_draw_difftotal ~prev ~next:nl (idem, theirs, (plot_elt Ours ours n), outc) (r n::diff_list)
      |[],[] -> SvgPainter.(add_to theirs (add_to ours (add_to idem outc))), diff_list
    in
    let new_ctx = SvgPainter.new_from out_canevas in
    rec_draw_difftotal ~prev ~next (new_ctx, new_ctx, new_ctx, out_canevas) []

    let display_diff ~from_ctx ~to_ctx (filename:string list) ~keep =
      let prev = List.sort ~compare from_ctx in
      let next = List.sort ~compare to_ctx in
      match
        draw_difftotal ~prev ~next (SvgPainter.get_color_context c z)
      with
      | _, [] ->
        Lwt.return false
      | outctx, diff_list ->
        let merged_rects = merge_rects diff_list in
        let outctx = List.fold_left ~f:draw_bb ~init:outctx merged_rects in
        let svg_name = SysAbst.build_tmp_svg_name ~keep "diff_" filename in
        let open UnixLabels in
        let wait_for_1_s result =
          match result with
          | WSIGNALED n ->
            Printf.printf "signalled with signal %d\n" n ;
            Lwt.return svg_name
          | WSTOPPED n ->
            Printf.printf "stopped with %d\n" n ;
            Lwt.return svg_name
          | WEXITED err -> (
              match err with
              | 127 ->
                Printf.printf "Command not found: %s\n" d ;
                Lwt.return svg_name
              | 0 ->
                let t, u = Lwt.wait () in
                let erase_timeout =
                  Lwt_timeout.create 1 (fun () -> Lwt.wakeup u svg_name)
                in
                Lwt_timeout.start erase_timeout ;
                t
              | _ ->
                Printf.printf "Errored with code %d\n" err ;
                Lwt.return svg_name )
        in
        Lwt_io.with_file ~mode:Lwt_io.Output svg_name (fun o ->
            Lwt_io.write o @@ SvgPainter.write ~op:false outctx )
        >>= fun _ ->
        SysAbst.exec d [|svg_name|]
        >>= wait_for_1_s
        >>= SysAbst.finalize_tmp_file ~keep
        >|= fun _ -> true
  end
  : Differ )

module SP = struct
  include S

  type painterContext = SvgPainter.t
end

module ImageDiff = struct
  let doc = "use compare (ImageMagick) between bitmaps"

  type pctx = SvgPainter.t

  module S = SP

  let display_diff ~from_ctx ~to_ctx filename ~keep =
    let from_filename = SysAbst.build_tmp_svg_name ~keep "from_" filename in
    let to_filename = SysAbst.build_tmp_svg_name ~keep "to_" filename in
    let both_files =
      List.map
        ~f:(fun (svg_name, context) ->
            Lwt_io.with_file ~mode:Lwt_io.Output svg_name (fun o ->
                Lwt_io.write o (SvgPainter.write context) ) )
        [(from_filename, from_ctx); (to_filename, to_ctx)]
    in
    let both = Lwt.join both_files in
    let compare_them =
      both
      >>= fun _ ->
      SysAbst.exec "git-imgdiff" [|from_filename; to_filename|]
      >|= let open UnixLabels in
      function
      | WEXITED ret ->
        if Int.equal ret 0 then true else false
      | WSIGNALED _ ->
        false
      | WSTOPPED _ ->
        false
    in
    let%lwt ret =
      try%lwt compare_them with
      | GitFs.InternalGitError s ->
        Lwt_io.printf "%s\n" s >|= fun () -> false
      | _ ->
        Lwt_io.printf "unknown error\n" >|= fun () -> false
    in
    Lwt.join
    @@ List.map
      ~f:(SysAbst.finalize_tmp_file ~keep)
      [from_filename; to_filename]
    >|= fun _ -> ret
end

let diff_cmd f t filename =
  let diff_cmd = [|"--no-pager"; "diff"; "--word-diff"|] in
  match (f, t) with
  | GitFS fc, GitFS tc ->
    ("git", Array.append diff_cmd [|fc; tc; "--"; filename|])
  | TrueFS _, GitFS tc ->
    ("git", Array.append diff_cmd [|tc; "--"; filename|])
  | GitFS fc, TrueFS _ ->
    ("git", Array.append diff_cmd [|fc; "--"; filename|])
  | TrueFS fc, TrueFS tc ->
    ( "diff"
    , [| fc ^ Filename.dir_sep ^ filename
       ; tc ^ Filename.dir_sep ^ filename |] )

let doit from_fs to_fs file_to_diff differ textdiff libs keep colors zone_color =
  let module_d =
    match differ with
    | Image_Diff ->
      (module ImageDiff : Differ)
    | Internal s ->
      internal_diff s colors zone_color
  in
  let module D = (val module_d : Differ) in
  let module F = (val (fs_mod from_fs) : Simple_FS) in
  let module T = (val (fs_mod to_fs) : Simple_FS) in
  let module FromP = FSPainter (D.S) (F) in
  let module ToP = FSPainter (D.S) (T) in
  let file_list =
    match file_to_diff with
    | None ->
      let from_list = FromP.find_schematics () in
      let to_list = ToP.find_schematics () in
      intersect_lists from_list to_list
    | Some filename ->
      let filename_l = String.split ~on:'/' filename in
      Lwt.return [filename_l]
  in
  let preload_libs () =
    Lwt_list.fold_left_s
      (fun c f -> Lwt_stream.fold D.S.add_lib (Lwt_io.lines_of_file f) c)
      (D.S.initial_context ()) libs
  in
  let from_init_ctx = FromP.context_from @@ preload_libs () in
  let to_init_ctx = ToP.context_from @@ preload_libs () in
  let compare_one filename =
    let%lwt from_ctx = FromP.process_file from_init_ctx filename in
    let%lwt to_ctx = ToP.process_file to_init_ctx filename in
    match%lwt D.display_diff ~from_ctx ~to_ctx filename ~keep with
    | true ->
      Lwt.return ()
    | false ->
      if textdiff then
        let cmd, args = diff_cmd F.label T.label @@ String.concat ~sep:"/" filename in
        SysAbst.exec cmd args >|= ignore
      else Lwt.return ()
  in
  let compare_all = file_list >>= Lwt_list.map_p compare_one >|= to_unit in
  let catch_errors =
    Lwt.catch
      (fun _ ->
         Lwt_io.printf "%s between %s and %s\n" D.doc (doc from_fs) (doc to_fs)
         >>= fun _ -> compare_all )
      (function
        | GitFs.InternalGitError s ->
          Lwt_io.printf "Git Exception: %s\n" s
        | a ->
          Lwt_io.printf "Exception %s\n" (Exn.to_string a) )
  in
  Lwt_main.run catch_errors
