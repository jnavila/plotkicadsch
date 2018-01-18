open Core_kernel
open Lwt.Infix
open Git_unix
module Search = Git.Search.Make(FS)

module S = Kicadsch.MakeSchPainter(SvgPainter)
open Kicadsch.Sigs

module type Simple_FS = sig
  val doc: string
  val get_content: string list -> string Lwt.t
  val list_files: (string -> bool) -> (string * string) list Lwt.t
end

exception InternalGitError of string

let git_fs commitish =
  (module
  struct
    let rev_parse r =
      let open Lwt_process in
      pread ~stderr:`Dev_null ("", [|"git" ;"rev-parse"; r ^ "^{commit}"|]) >>= (fun s ->
          try
            Lwt.return @@ Git_unix.Hash_IO.of_hex @@ String.prefix s 40
          with
            _ -> Lwt.fail (InternalGitError ("cannot parse rev " ^ r)))

    let doc = "Git rev " ^ commitish
    let fs = FS.create ~root:(Sys.getcwd ()) ()
    let theref = rev_parse commitish
    let with_path path action =
      fs >>= fun t ->
      theref >>= fun h ->
      Search.find t h (`Commit(`Path path)) >>= function
         | None     -> Lwt.fail(InternalGitError ("path not found: /" ^ (String.concat ~sep:"/" path)))
         | Some sha ->
           FS.read t sha >>= function
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
    let rootname = rootname
    let get_content filename = Lwt_io.with_file ~mode:Lwt_io.input (String.concat ~sep:"/" filename) (fun x -> Lwt_io.read x)
    let hash_file filename = get_content [filename] >|= fun c ->
      let blob_content = (Printf.sprintf "blob %d\000" (String.length c)) ^ c in
                             filename, (Sha1.to_hex (Sha1.string blob_content))
    let list_files pattern =
      let all_files  = (Lwt_unix.files_of_directory rootname) in
      let matched_files = Lwt_stream.filter pattern all_files in
      let decorated_files = Lwt_stream.map_s (fun name -> (hash_file name)) matched_files in
      Lwt_stream.to_list decorated_files
  end: Simple_FS)

let ends_with s e =
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
  val context_from: unit -> S.schContext Lwt.t
end =
struct
  let find_schematics () = F.list_files (fun name -> ends_with name ".sch")
  let process_file initctx filename =
    let parse c l = S.parse_line l c in
    initctx >>= fun init ->
    F.get_content [filename] >|= fun ctt ->
    let lines = String.split_on_chars ~on:['\n'] ctt in
    let endctx = List.fold_left ~f:parse ~init lines in
    S.output_context endctx

  let find_libs () =
    F.list_files (fun name -> ends_with name "-cache.lib")  >|= List.map ~f:(fun (n, _) -> n)

  let read_libs _ lib_list  =
  Lwt_list.fold_left_s (fun c l ->
      F.get_content [l] >|=
      String.split_on_chars ~on:['\n'] >|=
      List.fold_left ~f:(fun ctxt l -> S.add_lib l ctxt) ~init:c) (S.initial_context () ) lib_list

  let context_from () = find_libs () >>= read_libs (S.initial_context ())
end

let intersect_lists l1l l2l =
  l1l >>= fun l1 ->
  l2l >|= fun l2 -> (
    List.filter ~f:(fun (name2, sha2) ->
        (List.exists ~f:(fun (name1, sha1) ->
             ((String.equal name1 name2) && (not(String.equal sha2 sha1)))) l1)) l2 |>
    List.map ~f:(fun (n, _) -> n))

let to_unit _ = ()

let delete_file fnl =
  Lwt_unix.unlink fnl

let build_svg_name aprefix aschname =
  aprefix ^ (String.sub aschname ~pos:0 ~len:(String.length aschname - 4)) ^ ".svg"

module type Differ =  sig
  val doc: string
  type pctx
  module S : SchPainter with type painterContext = pctx
  val display_diff: from_ctx:pctx -> to_ctx:pctx -> string -> unit Lwt.t
end

let internal_diff (d:string) = (
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
        | Rect (_,  Coord (x1, y1), Coord (x2, y2)) -> Printf.sprintf "rectangle %d %d -> %d %d" x1 y1 x2 y2
        | Circle (_, Coord (x, y), radius) -> Printf.sprintf "circle %d %d %d" x y radius
        | Arc (_ ,Coord (x1, y1), Coord (x2, y2), radius) -> Printf.sprintf "arc %d %d -> %d %d %d" x1 y1 x2 y2 radius
        | Image (Coord (x, y), scale , _) -> Printf.sprintf "image %d %d %f" x y scale
        | Format (Coord(x, y)) -> Printf.sprintf "format %d %d" x y
      )

    type diff_style = Theirs | Ours | Idem

    let plot_elt style  out_ctx (arg: ListPainter.t) =
      ListPainter.(
        let module O = SvgPainter in
        let kolor = match style with
          | Theirs -> Kicadsch.Sigs.Red
          | Ours -> Kicadsch.Sigs.Green
          | Idem -> Kicadsch.Sigs.Black in
        match arg with
        | Text (_, text, o, c, s, j, style) -> O.paint_text ~kolor text o c s j style out_ctx
        | Line (_, s, from_, to_) -> O.paint_line ~kolor ~width:s from_ to_ out_ctx
        | Rect (_,  c1, c2) -> O.paint_rect c1 c2 out_ctx
        | Circle (_, center, radius) -> O.paint_circle center radius out_ctx
        | Arc (_ ,start_, end_, radius) -> O.paint_arc start_ end_ radius out_ctx
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

    let display_diff ~from_ctx ~to_ctx filename =
      let from_canevas = Array.of_list from_ctx in
      let to_canevas = Array.of_list to_ctx in
      match draw_difftotal ~mine:from_canevas ~other:to_canevas (SvgPainter.get_context ()) with
      | None -> Lwt.return ()
      | Some outctx ->
        let svg_name = build_svg_name "diff_" filename in
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
        Lwt_process.exec ("", [| d; svg_name |]) >>=
        wait_for_1_s >>=
        delete_file

  end :Differ)

module SP = struct
  include S
  type painterContext = SvgPainter.t
end

module ImageDiff = struct
  let doc = "use compare (ImageMagick) between bitmaps"
  type pctx = SvgPainter.t
  module S = SP
  let display_diff ~from_ctx ~to_ctx filename =
    let from_filename = build_svg_name "from_" filename in
    let to_filename = build_svg_name "to_" filename in
    let both_files =
      List.map ~f:(
        fun (svg_name, context) -> Lwt_io.with_file ~mode:Lwt_io.Output svg_name (fun o ->
            Lwt_io.write o (SvgPainter.write context)))
        [(from_filename, from_ctx); (to_filename, to_ctx)] in
    let both = Lwt.join both_files in
    let compare_them =
      both >>= fun _ ->
       Lwt_process.exec ("", [| "git-imgdiff"; from_filename ; to_filename|]) >|= to_unit in
    Lwt.join @@
      List.map ~f:(fun f ->
          Lwt.catch
            (fun () ->
              compare_them >>= fun () ->
              delete_file f)
            (fun exc ->
              begin
              match exc with
             | InternalGitError s -> Printf.printf "%s\n" s
             | _ -> Printf.printf "unknown error\n"
              end; delete_file f))
        [from_filename; to_filename]
end

let doit from_fs to_fs differ =
  let module D = (val differ : Differ) in
  let module F = (val from_fs: Simple_FS) in
  let module T = (val to_fs: Simple_FS) in
  let module FromP = FSPainter (D.S) (F) in
  let module ToP = FSPainter (D.S) (T) in
  let from_list = FromP.find_schematics () in
  let to_list = ToP.find_schematics () in
  let file_list = intersect_lists from_list to_list in
  let from_init_ctx = FromP.context_from () in
  let to_init_ctx = ToP.context_from () in
  let compare_one filename =
    FromP.process_file from_init_ctx filename >>= fun from_ctx ->
    ToP.process_file to_init_ctx filename >>= fun to_ctx ->
    D.display_diff ~from_ctx ~to_ctx filename in
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
  if String.sub s ~pos:0 ~len:4 = "dir:" then
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
  let module D = (val differ: Differ) in
  Format.fprintf out "%s" D.doc

let differ =
  let docv = "diff strategy used" in
  Arg.(conv ~docv ((fun d -> Result.Ok(internal_diff d)), pp_differ))

let internal_diff =
  let doc = "use an internal diff algorithm and use the $(docv) to display the result." in
  let docv = "BROWSER" in
  Arg.(value & opt ~vopt:(internal_diff "chromium") differ (module ImageDiff:Differ) & info ["i"; "internal"] ~doc ~docv)

let plotgitsch_t = Term.(const doit $ from_ref $ to_ref $ internal_diff)

let info =
  let doc = "Show graphically the differences between two git revisions of a kicad schematic" in
  let man = [
    `S Manpage.s_bugs;
    `P "Email bug reports to https//github.com/jnavila/plotkicadsch/issues" ]
  in
  Term.info "plotgitsch" ~version:"%‌%VERSION%%" ~doc ~exits:Term.default_exits ~man

let () = Term.exit @@ Term.eval (plotgitsch_t, info)
