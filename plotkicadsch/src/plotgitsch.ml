open Core_kernel
open Lwt.Infix

module S = Kicadsch.MakeSchPainter(SvgPainter)
open Kicadsch.Sigs

type fs_type = TrueFS of string | GitFS of string
type differ = Internal of string | Image_Diff
module type Simple_FS = sig
  val doc: string
  val label: fs_type
  val get_content: string list -> string Lwt.t
  val list_files: (string -> bool) -> (string * string) list Lwt.t
end

exception InternalGitError of string

let git_fs commitish =
  (module
  struct
    open Git_unix
    module Search = Git.Search.Make(FS)
    let rev_parse r =
      SysAbst.pread "git" [|"rev-parse"; r ^ "^{commit}"|] >>= (fun s ->
        try
          Lwt.return @@ Git_unix.Hash_IO.of_hex @@ String.prefix s 40
        with
          _ -> Lwt.fail (InternalGitError ("cannot parse rev " ^ r)))

    let doc = "Git rev " ^ commitish
    let label = GitFS commitish
    let git_root =
      let open Filename in
      let rec recurse (d,b)  =
        let new_gitdir = concat d ".git/description" in
        try%lwt
          let%lwt _ = Lwt_unix.stat new_gitdir in
          (* that's a git repo and d is the root *)
          Lwt.return (d, b)
        with
        | Unix.Unix_error (Unix.ENOENT,_,_) ->
        let new_d = dirname d in
        if (String.equal new_d d) then
          (* we've reached the root of the FS *)
          Lwt.fail (InternalGitError "not in a git repository")
        else
          let new_b = (basename d) :: b in
          recurse (new_d, new_b)
        | e -> raise e
      in recurse @@(Sys.getcwd (), [])

    let fs = git_root >>= fun (root, _) -> FS.create ~root ()

    let theref = rev_parse commitish

    let with_path path action =
      let%lwt t = fs in
      let%lwt h = theref in
      let%lwt _, rel_path = git_root in
      match%lwt Search.find t h (`Commit(`Path (List.concat [rel_path; path ]))) with
         | None     -> Lwt.fail(InternalGitError ("path not found: /" ^ (String.concat ~sep:Filename.dir_sep path)))
         | Some sha ->
           match%lwt FS.read t sha with
           | Some a -> action a
           | None -> Lwt.fail (InternalGitError "sha not found")

    let get_content filename =
      with_path filename @@ function
      | Git.Value.Blob b -> Lwt.return (Git.Blob.to_raw b)
      | _ -> Lwt.fail(InternalGitError "not a valid path")

    let find_file filter t =
      let open Git.Tree in
      t |>
      List.filter ~f:(fun e -> filter e.name) |>
      List.map ~f:(fun e -> e.name, (Git.Hash.to_hex e.node))

    let list_files pattern =
      with_path [] @@ function
      | Git.Value.Tree t -> Lwt.return @@ find_file pattern t
      | _ -> Lwt.fail (InternalGitError "not a tree!")
  end: Simple_FS)

let true_fs rootname =
  (module
  struct
    let doc = "file system " ^ rootname
    let label = TrueFS rootname
    let rootname = rootname
    let get_content filename = Lwt_io.with_file ~mode:Lwt_io.input (String.concat ~sep:Filename.dir_sep filename) Lwt_io.read
    let hash_file filename = get_content [filename] >|= fun c ->
      let blob_content = (Printf.sprintf "blob %d\000" (String.length c)) ^ c in
                             filename, (Sha1.to_hex (Sha1.string blob_content))
    let list_files pattern =
      let all_files  = Lwt_unix.files_of_directory rootname in
      let matched_files = Lwt_stream.filter pattern all_files in
      let decorated_files = Lwt_stream.map_s hash_file matched_files in
      Lwt_stream.to_list decorated_files
  end: Simple_FS)

let ends_with e s =
  let ls = String.length s in
  let le = String.length e in
  if ls < le then
    false
  else
    let rec loop s e i =
      if i = le then true else
      if String.get s (ls - le + i) <> String.get e i then false
      else loop s e (i+1)
    in
    loop s e 0

module L = Kicadsch.MakeSchPainter(ListPainter.L)
module LP = struct
  include L
  type painterContext=ListPainter.listcanevas
end

module FSPainter (S: SchPainter) (F: Simple_FS) : sig
  val find_schematics: unit -> (string*string) list Lwt.t
  val process_file: S.schContext Lwt.t -> string -> S.painterContext Lwt.t
  val context_from: S.schContext Lwt.t -> S.schContext Lwt.t
end =
struct
  let find_schematics () = F.list_files (ends_with ".sch")
  let process_file initctx filename =
    let parse c l = S.parse_line l c in
    let%lwt init = initctx in
    F.get_content [filename] >|= fun ctt ->
    let lines = String.split_on_chars ~on:['\n'] ctt in
    let endctx = List.fold_left ~f:parse ~init lines in
    S.output_context endctx

  let find_libs () =
    F.list_files (ends_with "-cache.lib")  >|= List.map ~f:(fun (n, _) -> n)

  let read_libs initial_ctx lib_list  =
  Lwt_list.fold_left_s (fun c l ->
      F.get_content [l] >|=
      String.split_on_chars ~on:['\n'] >|=
      List.fold_left ~f:(fun ctxt l -> S.add_lib l ctxt) ~init:c) (initial_ctx) lib_list

  let context_from from_ctx =
    let%lwt initial_context = from_ctx in
    find_libs () >>= read_libs (initial_context)
end

let intersect_lists l1l l2l =
  l1l >>= fun l1 ->
  l2l >|= fun l2 -> (
    List.filter ~f:(fun (name2, sha2) ->
        (List.exists ~f:(fun (name1, sha1) ->
             ((String.equal name1 name2) && (not(String.equal sha2 sha1)))) l1)) l2 |>
    List.map ~f:(fun (n, _) -> n))

let to_unit _ = ()

let build_svg_name aprefix aschname =
  aprefix ^ (String.sub aschname ~pos:0 ~len:(String.length aschname - 4)) ^ ".svg"

module type Differ =  sig
  val doc: string
  type pctx
  module S : SchPainter with type painterContext = pctx
  val display_diff: from_ctx:pctx -> to_ctx:pctx -> string -> keep:bool -> bool Lwt.t
end

let internal_diff (d:string) (c: SvgPainter.diff_colors option) = (
  module struct
    let doc = "internal diff and show with "^d
    type pctx = ListPainter.listcanevas
    module S = LP

    module Patdiff = Patience_diff_lib.Patience_diff.Make(String)

    let transform (arg: ListPainter.t) =
      ListPainter.(
        match arg with
        | Text (_, text, _o, Coord (x, y), Size s, _j, _style) -> Printf.sprintf "text %s %d %d %d" text x y s
        | Line (_, Size s, Coord (x1, y1), Coord (x2, y2)) -> Printf.sprintf "line %d %d -> %d %d %d" x1 y1 x2 y2 s
        | Rect (_, _,  Coord (x1, y1), Coord (x2, y2)) -> Printf.sprintf "rectangle %d %d -> %d %d" x1 y1 x2 y2
        | Circle (_, _, Coord (x, y), radius) -> Printf.sprintf "circle %d %d %d" x y radius
        | Arc (_, _ , Coord (x, y), Coord (x1, y1), Coord (x2, y2), radius) -> Printf.sprintf "arc %d %d -> %d %d %d %d %d" x1 y1 x2 y2 radius x y
        | Image (Coord (x, y), scale , _) -> Printf.sprintf "image %d %d %f" x y scale
        | Format (Coord(x, y)) -> Printf.sprintf "format %d %d" x y
      )

    type diff_style = Theirs | Ours | Idem

    let plot_elt style  out_ctx (arg: ListPainter.t) =
      ListPainter.(
        let module O = SvgPainter in
        let kolor = match style with
          | Theirs -> `Old
          | Ours -> `New
          | Idem -> `ForeGround in
        match arg with
        | Text (_, text, o, c, s, j, style) -> O.paint_text ~kolor text o c s j style out_ctx
        | Line (_, s, from_, to_) -> O.paint_line ~kolor ~width:s from_ to_ out_ctx
        | Rect (_, _,  c1, c2) -> O.paint_rect ~kolor c1 c2 out_ctx
        | Circle (_, _, center, radius) -> O.paint_circle ~kolor center radius out_ctx
        | Arc (_, _ , center, start_, end_, radius) -> O.paint_arc ~kolor center start_ end_ radius out_ctx
        | Image (corner, scale , data) -> O.paint_image corner scale data out_ctx
        | Format (Coord (x, y)) -> O.set_canevas_size x y out_ctx
      )

    let draw_range ctx r = Patience_diff_lib.Patience_diff.Range.(
        match r with
        | Same a -> Array.fold ~f:(fun c (x, _) -> plot_elt Idem c x) a ~init:ctx
        | Old a  ->  Array.fold ~f:(plot_elt Theirs) a ~init:ctx
        | New a  ->  Array.fold ~f:(plot_elt Ours) a ~init:ctx
        | Replace (o,n) -> let c' =Array.fold ~f:(plot_elt Ours) n ~init:ctx in
                           Array.fold o ~f:(plot_elt Theirs) ~init:c'
        | Unified a -> Array.fold ~f:(plot_elt Idem) a ~init:ctx
      )

    type hunk = ListPainter.t Patience_diff_lib.Patience_diff.Hunk.t

    let draw_hunk (h: hunk) ctx =
      List.fold_left ~f:draw_range ~init:ctx h.ranges

    let draw_difftotal ~other ~mine out_canevas =
      let comparison = Patdiff.get_hunks ~transform ~mine ~other ~context:5 ~big_enough:1 in
      if List.for_all ~f:Patience_diff_lib.Patience_diff.Hunk.all_same comparison then
        None
      else
        let draw_all_hunk (ctx, n) (h: hunk)=
          let mine_to_plot = h.mine_start - n - 1 in
          (Array.fold ~f:(plot_elt Idem) (Array.sub mine ~pos:n ~len:mine_to_plot) ~init:ctx|>draw_hunk h) , (h.mine_start + h.mine_size - 2) in
        let ctx, n = List.fold ~f:draw_all_hunk ~init:(out_canevas, 0) comparison in
        Some (Array.fold ~f:(plot_elt Idem) (Array.sub mine ~pos:n ~len:(Array.length mine - n)) ~init:ctx)

    let display_diff ~from_ctx ~to_ctx filename ~keep =
      let from_canevas = Array.of_list from_ctx in
      let to_canevas = Array.of_list to_ctx in
      match draw_difftotal ~mine:from_canevas ~other:to_canevas (SvgPainter.get_color_context c) with
      | None -> Lwt.return false
      | Some outctx ->
         let svg_name = SysAbst.build_tmp_svg_name ~keep "diff_" filename in
         let keep_as = if keep then Some (build_svg_name "diff_" filename) else None in
        let open Unix in
        let wait_for_1_s result =  match result with
            | WSIGNALED n -> Printf.printf "signalled with signal %d\n" n;Lwt.return svg_name
            | WSTOPPED n -> Printf.printf "stopped with %d\n" n; Lwt.return svg_name
            | WEXITED err ->
              (match err with
              | 127 -> Printf.printf "Command not found: %s\n" d; Lwt.return svg_name
              | 0 ->
                begin
                  let t, u = Lwt.wait () in
                  let erase_timeout = Lwt_timeout.create 1 (fun () -> Lwt.wakeup u svg_name) in
                  (Lwt_timeout.start erase_timeout;t)
                end
              | _ -> Printf.printf "Errored with code %d\n" err; Lwt.return svg_name)
        in
        Lwt_io.with_file ~mode:Lwt_io.Output svg_name ( fun o ->
            Lwt_io.write o @@ SvgPainter.write ~op:false outctx) >>= fun _ ->
        SysAbst.exec d [| svg_name |] >>=
        wait_for_1_s >>= SysAbst.finalize_tmp_file ~keep_as >|= fun _ -> true
  end :Differ)

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
      List.map ~f:(
        fun (svg_name, context) -> Lwt_io.with_file ~mode:Lwt_io.Output svg_name (fun o ->
            Lwt_io.write o (SvgPainter.write context)))
        [(from_filename, from_ctx); (to_filename, to_ctx)] in
    let both = Lwt.join both_files in
    let compare_them =
      both >>= fun _ ->
      SysAbst.exec  "git-imgdiff" [|from_filename ; to_filename|] >|=
        Unix.(function
        | WEXITED ret -> if (Int.equal ret 0) then true else false
        | WSIGNALED _ -> false
        | WSTOPPED _ -> false)
    in
    let%lwt ret =
      try%lwt
            compare_them
      with
      | InternalGitError s ->Lwt_io.printf "%s\n" s >|= fun () -> false
      | _ -> Lwt_io.printf "unknown error\n" >|= fun () ->  false
    in
    Lwt.join @@
        List.map ~f:(SysAbst.finalize_tmp_file ~keep_as:None) [from_filename; to_filename] >|= fun _ -> ret
end

let doit from_fs to_fs differ textdiff libs keep colors =
  let module_d = match differ with
    | Image_Diff -> (module ImageDiff: Differ)
    | Internal s -> internal_diff s colors in
  let module D = (val module_d: Differ) in
  let module F = (val from_fs: Simple_FS) in
  let module T = (val to_fs: Simple_FS) in
  let module FromP = FSPainter (D.S) (F) in
  let module ToP = FSPainter (D.S) (T) in
  let from_list = FromP.find_schematics () in
  let to_list = ToP.find_schematics () in
  let file_list = intersect_lists from_list to_list in

  let preload_libs () =
    Lwt_list.fold_left_s (fun c f -> Lwt_stream.fold D.S.add_lib (Lwt_io.lines_of_file f) c) (D.S.initial_context () ) libs in

  let from_init_ctx = FromP.context_from @@ preload_libs () in
  let to_init_ctx = ToP.context_from @@ preload_libs () in
  let compare_one filename =
    let%lwt from_ctx = FromP.process_file from_init_ctx filename in
    let%lwt  to_ctx = ToP.process_file to_init_ctx filename in
    begin
      match%lwt D.display_diff ~from_ctx ~to_ctx filename ~keep with
    | true ->  Lwt.return ()
    | false ->
       if textdiff then
         let diff_cmd = [|"--no-pager"; "diff"; "--word-diff"|] in
         let cmd, args =
           match F.label,T.label with
           | GitFS fc, GitFS tc -> "git",  Array.append diff_cmd[| fc; tc; "--"; filename |]
           | TrueFS _, GitFS tc -> "git", Array.append diff_cmd[| tc; "--"; filename |]
           | GitFS fc, TrueFS _ -> "git", Array.append diff_cmd[| fc; "--"; filename |]
           | TrueFS fc, TrueFS tc -> "diff", [|fc^Filename.dir_sep^filename; tc^Filename.dir_sep^filename|]
         in SysAbst.exec cmd args >|= ignore
       else Lwt.return ()
    end
  in
  let compare_all =  file_list >>= Lwt_list.map_p compare_one >|= to_unit in
  let catch_errors = Lwt.catch
      (fun _ ->   Lwt_io.printf "%s between %s and %s\n" D.doc F.doc T.doc >>= fun _ ->
        compare_all)
      (function
        | InternalGitError s -> Lwt_io.printf "Git Exception: %s\n" s
        | a -> Lwt_io.printf "Exception %s\n" (Exn.to_string a)) in
  Lwt_main.run catch_errors

open Cmdliner

let pp_fs out fs =
  let module FS = (val fs:Simple_FS) in
  Format.fprintf out "%s" FS.doc

let get_fs s =
  if (String.length s > 4) && (String.sub s ~pos:0 ~len:4 = "dir:") then
    true_fs (String.sub s ~pos:4 ~len:(String.length s - 4))
  else
    git_fs s

let reference =
  let docv = "a commitish reference" in
  Arg.(conv ~docv ((fun s -> Result.Ok (get_fs s)), pp_fs))

let from_ref =
  let doc = "reference from which the diff is performed. If it starts with 'dir:' it's a file system dir." in
  let docv = "FROM_REF" in
  Arg.(value & pos 0 reference (git_fs "HEAD") & info [] ~doc ~docv)

let to_ref =
  let doc = "target reference to diff with. If it starts with 'dir:' it's a file system dir." in
  let docv = "TO_REF" in
  Arg.(value & pos 1 reference ((true_fs ".")) & info [] ~doc ~docv)

let pp_differ out differ =
  let s = match differ with
  | Internal p -> "internal with viewer " ^ p
  | Image_Diff -> "external" in
  Format.fprintf out "%s" s

let differ =
  let docv = "diff strategy used" in
  Arg.(conv ~docv ((fun d -> Result.Ok(Internal d)), pp_differ))

let internal_diff =
  let doc = "use an internal diff algorithm and use the $(docv) to display the result." in
  let docv = "BROWSER" in
  Arg.(value & opt ~vopt:(Internal (SysAbst.default_opener ())) differ Image_Diff & info ["i"; "internal"] ~doc ~docv)

let preloaded_libs =
  let doc = "preload symbol library $(docv) in order to prepare the diff. This option can be used several times on command line." in
  let docv = "LIB" in
  Arg.(value & opt_all file [] & info ["l";"lib"] ~doc ~docv)

let textual_diff =
  let doc = "fall back to show a text diff if files are different but generate no visual diffs" in
  Arg.(value & flag & info ["t";"textdiff"] ~doc)

let keep_files =
  let doc = "by default, the svg diff files are deleted after launching the viewer; this option lets the files in place after viewing them. " in
  Arg.(value & flag & info ["k";"keep"] ~doc)

let pp_colors out c =
  let open SvgPainter in
  match c with
  | None -> Format.fprintf out "default colors"
  | Some {old_ver; new_ver; fg; bg} -> Format.fprintf out "%s:%s:%s:%s" old_ver new_ver fg bg

let extract_colors s =
  let open SvgPainter in
  let col_exp = "([0-9a-fA-F]{6})" in
  let cols_exp = "^" ^col_exp ^ ":" ^ col_exp ^ ":" ^ col_exp ^ ":" ^ col_exp ^ "$" in
  let col_re = Re.Posix.compile_pat cols_exp in
  match (Re.all col_re s) with
  | [ m ] ->
     begin
       match (Re.Group.all m) with
       | [| _ ; o; n; f; b |] ->
          let e c = "#" ^ c in
          Result.Ok (Some {old_ver = e o; new_ver = e n; fg = e f; bg = e b})
       | _ -> Result.Error (`Msg "wrong colors format")
     end
  | _ ->  Result.Error (`Msg "wrong colors format")

let get_colors =
  let docv = "scheme of colors for diffing" in
  Arg.(conv ~docv (extract_colors, pp_colors))

let colors =
  let doc = "list of colon separated hex RGB codes for colors used for diffing e.g. the default colors are FF0000:00FF00:00000:FFFFFF" in
  let docv = "old:new:foreground:background" in
  Arg.(value & opt get_colors None & info ["c"; "colors"] ~doc ~docv)

let plotgitsch_t = Term.(const doit $ from_ref $ to_ref $ internal_diff $ textual_diff $ preloaded_libs $ keep_files $ colors)


let info =
  let doc = "Show graphically the differences between two git revisions of a kicad schematic" in
  let man = [
    `S Manpage.s_bugs;
    `P "Open issues to https://github.com/jnavila/plotkicadsch/issues" ]
  in
  Term.info "plotgitsch" ~version:"%%VERSION%%" ~doc ~exits:Term.default_exits ~man

let () = Term.exit @@ Term.eval (plotgitsch_t, info)
