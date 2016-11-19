open Lwt.Infix
open Git_unix
module Search = Git.Search.Make(FS)

module SvgSchPainter = Kicadsch.MakeSchPainter(SvgPainter)
open SvgSchPainter

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

let find_file filter map t =
  let open Git.Tree in
  t |>
    List.filter (fun e -> filter e.name) |>
    List.map map

let filter_cache_libs  =
  let open Git.Tree in
  find_file (fun name -> ends_with name "-cache.lib") (fun e -> e.name)

let filter_schematics =
  let open Git.Tree in
  find_file (fun name -> ends_with name ".sch") (fun e -> e.name, e.node)

let fs = FS.create ()

exception InternalGitError of string

let with_path theref path action =
  fs >>= fun t ->
  theref >>= fun h ->
  Search.find t h (`Commit(`Path path)) >>= function
  | None     -> Lwt.fail(InternalGitError ("path not found: /" ^ (String.concat "/" path)))
  | Some sha ->
     FS.read t sha >>= function
     | Some a -> action a
     | None -> Lwt.fail (InternalGitError "sha not found")

let get_files theref pattern =
  with_path theref [] @@ function
     | Git.Value.Tree t -> Lwt.return @@ pattern t
     | _ -> Lwt.fail (InternalGitError "not a tree!")

let find_libs theref = get_files theref filter_cache_libs

let find_schematics theref = get_files theref filter_schematics

let read_file file theref =
  with_path theref file @@ function
    | Git.Value.Blob b -> Lwt.return (Git.Blob.to_raw b)
    | _ -> Lwt.fail(InternalGitError "not a valid path")

let read_libs context theref lib_list  =
  Lwt_list.map_p (fun l -> read_file [l] theref) lib_list >|= (fun contents ->
  let content = String.concat "\n"  contents in
  let lines = Str.split (Str.regexp "\n") content in
  Lwt_stream.of_list lines) >>= fun sl ->
  add_lib sl context

let intersect_lists l1l l2l =
  l1l >>= fun l1 ->
  l2l >|=
  List.filter (fun (name2, sha2) -> (List.exists (fun (name1, sha1) -> ((String.equal name1 name2) && (sha2 <> sha1))) l1))  >|=
    List.map (fun (n,s) -> n)


let process_file initctx svg_name content =
  initctx >>= fun init ->
  content >|= Str.split (Str.regexp "\n") >>= fun lines ->
  Lwt_stream.fold parse_line (Lwt_stream.of_list lines) init >>= fun endcontext ->
  Lwt_io.with_file Lwt_io.Output svg_name (fun o -> output_context endcontext o)

let rev_parse r =
  let open Lwt_process in
  pread ~stderr:`Dev_null ("", [|"git" ;"rev-parse"; r ^ "^{commit}"|]) >>= (fun s ->
    try Lwt.return @@ Git_unix.Hash_IO.of_hex @@ Str.first_chars s 40 with exn -> Lwt.fail (InternalGitError ("cannot parse rev " ^ r)))

let to_unit l = ()

let delete_file fnl =
  fnl >>=  fun fn -> Lwt_unix.unlink fn

let build_svg_name aref aschname =
  let fileout = (String.sub aschname 0 (String.length aschname - 4)) ^ ".svg" in
  aref >|= fun r -> (Git.Hash.to_hex r) ^ "_" ^ fileout

let doit () =
  let from_ref = rev_parse Sys.argv.(1) in
  let to_ref = rev_parse Sys.argv.(2) in
  let from_context = find_libs from_ref >>= read_libs (initial_context ()) from_ref in
  let to_context = find_libs to_ref >>= read_libs (initial_context ()) to_ref in
  let from_list = find_schematics from_ref in
  let to_list = find_schematics to_ref in
  let file_list = intersect_lists from_list to_list in
  let display_diff filename =
    let from_content = read_file [filename] from_ref in
    let to_content = read_file [filename] to_ref in
    let from_filename = build_svg_name from_ref filename in
    let to_filename = build_svg_name to_ref filename in
    let first = from_filename >>= fun n -> process_file from_context n from_content in
    let second = to_filename >>= fun n -> process_file to_context n to_content in
    let both = Lwt.join [ first; second] in
    let compare_them =
      from_filename >>= fun fname ->
      to_filename >>= fun tname ->
      both >>= fun _ ->
      Lwt_process.exec ("", [| "git-imgdiff"; fname ; tname|]) >|= to_unit in
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
  if Array.length Sys.argv = 3 then
    doit ()
  else
    Printf.printf "%s needs 2 revs to compare\n" Sys.argv.(0); exit 3
