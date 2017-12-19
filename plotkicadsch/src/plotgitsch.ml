open Lwt.Infix
open Git_unix
module Search = Git.Search.Make(FS)

module SvgSchPainter = Kicadsch.MakeSchPainter(SvgPainter)
open SvgSchPainter

module type Simple_FS = sig
  val get_content: string list -> string Lwt.t
  val list_files: (string -> bool) -> (string * string) list Lwt.t
end

exception InternalGitError of string

let git_fs theref =
  (module
     struct
       let fs = FS.create ~root:(Sys.getcwd ()) ()
       let theref = theref
       let with_path path action =
         fs >>= fun t ->
         theref >>= fun h ->
         Search.find t h (`Commit(`Path path)) >>= function
         | None     -> Lwt.fail(InternalGitError ("path not found: /" ^ (String.concat "/" path)))
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
           List.filter (fun e -> filter e.name) |>
           List.map (fun e -> e.name, (Git.Hash.to_hex e.node))

       let list_files pattern =
         with_path [] @@ function
                        | Git.Value.Tree t -> Lwt.return @@ find_file pattern t
                        | _ -> Lwt.fail (InternalGitError "not a tree!")
  end: Simple_FS)



let true_fs rootname = (module struct
    let rootname = rootname
    let get_content filename = Lwt_io.with_file ~mode:Lwt_io.input (String.concat "/" filename) (fun x -> Lwt_io.read x)
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

let find_libs thefs =
  let module M = (val thefs: Simple_FS) in
  M.list_files (fun name -> ends_with name "-cache.lib")  >|= List.map (fun (n, _) -> n)

let find_schematics thefs =
    let module M = (val thefs: Simple_FS) in
  M.list_files (fun name -> ends_with name ".sch")

let read_libs _ thefs lib_list  =
  let module M = (val thefs: Simple_FS) in
  Lwt_list.fold_left_s (fun c l ->
      M.get_content [l] >|=
      Str.split (Str.regexp "\n") >|=
      List.fold_left (fun ctxt l -> add_lib l ctxt) c) (initial_context () ) lib_list

module L = ListPainter.ListPainter

let read_libs_l _ thefs lib_list  =
  let module M = (val thefs: Simple_FS) in
  Lwt_list.fold_left_s (fun c l ->
      M.get_content [l] >|=
      Str.split (Str.regexp "\n") >|=
      List.fold_left (fun ctxt l -> L.add_lib l ctxt) c) (L.initial_context () ) lib_list

let intersect_lists l1l l2l =
  l1l >>= fun l1 ->
  l2l >|= fun l2 -> (
  List.filter (fun (name2, sha2) -> (List.exists (fun (name1, sha1) -> ((String.equal name1 name2) && (not(String.equal sha2 sha1)))) l1)) l2 |>
    List.map (fun (n, _) -> n))



let process_file initctx svg_name content =
  initctx >>= fun init ->
  content >|= Str.split (Str.regexp "\n") >>= fun lines ->
  Lwt_stream.fold parse_line (Lwt_stream.of_list lines) init >>= fun endcontext ->
  Lwt_io.with_file ~mode:Lwt_io.Output svg_name (fun o -> Lwt_io.write o (SvgPainter.write @@ output_context endcontext))


let context_list initctx content =
  let parse c l = L.parse_line l c in
  initctx >>= fun init ->
  content >|= fun ctt ->
  let lines = String.split_on_char '\n' ctt in
  let endctx = List.fold_left parse init lines in
  let endcontext = L.output_context endctx in
  Array.of_list endcontext

let rev_parse r =
  let open Lwt_process in
  pread ~stderr:`Dev_null ("", [|"git" ;"rev-parse"; r ^ "^{commit}"|]) >>= (fun s ->
    try
      Lwt.return @@ Git_unix.Hash_IO.of_hex @@ Str.first_chars s 40
    with
      _ -> Lwt.fail (InternalGitError ("cannot parse rev " ^ r)))

let to_unit _ = ()

module Patdiff = Patience_diff_lib.Patience_diff.Make(Base.String)

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

let plot_elt style (arg: ListPainter.t) out_ctx=
  ListPainter.(
    let module O = SvgPainter in
    let kolor = match style with
      | Theirs -> Printf.printf "printing their ";Kicadsch.Sigs.Red
      | Ours -> Printf.printf "printing our ";Kicadsch.Sigs.Green
      | Idem -> Printf.printf "printing idem ";Kicadsch.Sigs.Black in
    match arg with
  | Text (_, text, o, c, s, j, style) -> Printf.printf "text %s\n" text;O.paint_text ~kolor text o c s j style out_ctx
  | Line (_, s, from_, to_) -> Printf.printf "line\n";O.paint_line ~kolor ~width:s from_ to_ out_ctx
  | Rect (_,  c1, c2) -> Printf.printf "rect\n";O.paint_rect c1 c2 out_ctx
  | Circle (_, center, radius) -> Printf.printf "circle\n";O.paint_circle center radius out_ctx
  | Arc (_ ,start_, end_, radius) -> Printf.printf "arc\n";O.paint_arc start_ end_ radius out_ctx
  | Image (corner, scale , data) -> Printf.printf "image\n";O.paint_image corner scale data out_ctx
  | Format (Coord (x, y)) -> Printf.printf "format %d %d\n" x y;O.set_canevas_size x y out_ctx
  )

let draw_range ctx r = Patience_diff_lib.Patience_diff.Range.(
    Printf.printf "Printing range ";
    match r with
    | Same a -> Printf.printf "Same\n";Array.fold_right (fun (x, _) -> plot_elt Idem x) a ctx
    | Old a  -> Printf.printf "Old\n"; Array.fold_right (plot_elt Theirs) a ctx
    | New a  -> Printf.printf "New\n"; Array.fold_right (plot_elt Ours) a ctx
    | Replace (n,o) -> Printf.printf "Replace\n";Array.fold_right (plot_elt Ours) n ctx |> Array.fold_right (plot_elt Theirs) o
    | Unified a -> Printf.printf "Unified\n";Array.fold_right (plot_elt Idem) a ctx
                       )
type hunk = ListPainter.t Patience_diff_lib.Patience_diff.Hunk.t

let draw_hunk (h: hunk) ctx =
  List.fold_left draw_range ctx h.ranges

let draw_difftotal other mine out_canevas =
  let comparison = Patdiff.get_hunks ~transform ~context:5 ~mine ~other in
  let draw_all_hunk (ctx, n) (h: hunk) =
    Printf.printf "plotting common from %d to %d over %d\n" n h.mine_start (Array.length mine - 1);
    (Array.fold_right (plot_elt Idem) (Array.sub mine n (h.mine_start - n - 1)) ctx|> draw_hunk h) , (h.mine_start + h.mine_size - 2) in
  let ctx, n = List.fold_left draw_all_hunk (out_canevas, 0) comparison in
  Printf.printf "plotting end common from %d to %d\n" n (Array.length mine - 1);
  Array.fold_right (plot_elt Idem) (Array.sub mine n (Array.length mine - n)) ctx

let delete_file fnl =
  Lwt_unix.unlink fnl

let build_svg_name aprefix aschname =
  aprefix ^ (String.sub aschname 0 (String.length aschname - 4)) ^ ".svg"

let context_from fs = find_libs fs >>= read_libs (initial_context ()) fs
let list_context fs = find_libs fs >>= read_libs_l (L.initial_context ()) fs


let doit_internal from_fs to_fs =
  let from_context = list_context from_fs in
  let to_context = list_context to_fs in
  let from_list = find_schematics from_fs in
  let to_list = find_schematics to_fs in
  let file_list = intersect_lists from_list to_list in
  let display_diff filename =
    let svg_name = filename ^".svg" in
    let get_canevas fs context =
          let module M = (val fs: Simple_FS) in
          let content = M.get_content[filename] in
          context_list context content
    in
    get_canevas from_fs from_context >>= fun from_canevas ->
    get_canevas to_fs to_context >>= fun to_canevas ->
    let outctx = draw_difftotal from_canevas to_canevas (SvgPainter.get_context ()) in
    Lwt_io.with_file ~mode:Lwt_io.Output svg_name (fun o -> Lwt_io.write o (SvgPainter.write outctx))
  in
  let compare_all = file_list >>= (Lwt_list.map_p display_diff) >|= to_unit in
  Lwt_main.run compare_all

let doit from_fs to_fs =
  let from_context = context_from from_fs in
  let to_context = context_from to_fs in
  let from_list = find_schematics from_fs in
  let to_list = find_schematics to_fs in
  let file_list = intersect_lists from_list to_list in
  let display_diff filename =
    let from_filename = build_svg_name "from_" filename in
    let to_filename = build_svg_name "to_" filename in
    let both_files =
      List.map (
          fun (fs, svg_name, context) ->
          let module M = (val fs: Simple_FS) in
          let content = M.get_content [filename] in
          process_file context svg_name content) [(from_fs, from_filename, from_context); (to_fs, to_filename, to_context)] in
    let both = Lwt.join both_files in
    let compare_them =
      both >>= fun _ ->
      Lwt_process.exec ("", [| "git-imgdiff"; from_filename ; to_filename|]) >|= to_unit in
    Lwt.join @@
      List.map (fun f ->
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
               [from_filename; to_filename] in
  let compare_all = file_list >>= (Lwt_list.map_p display_diff) >|= to_unit in
  Lwt_main.run compare_all

let () =
  match Array.length Sys.argv with
  | 1 -> let from_ref = rev_parse "HEAD" in
        doit_internal (git_fs from_ref) (true_fs ".")
  | 2 ->
    let from_ref = rev_parse Sys.argv.(1) in
    doit_internal (git_fs from_ref) ( true_fs ".")
  | 3 ->
     let from_ref = rev_parse Sys.argv.(1) in
     let to_ref = rev_parse Sys.argv.(2) in
     doit (git_fs from_ref) (git_fs to_ref)
  | _ ->
    Printf.printf "%s needs 0, 1 or 2 revs to compare\n" Sys.argv.(0); exit 3
