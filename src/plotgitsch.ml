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

let find_cache_libs t =
  let open Git.Tree in
  t |>
    List.filter (fun e -> ends_with e.name "-cache.lib") |>
    List.map (fun e -> e.name)

let fs = FS.create ()

let find_libs ref =
  fs >>= fun t ->
  ref >>= fun h ->
  Search.find t h (`Path []) >>= function
  | None     -> failwith "path not found"
  | Some sha ->
     FS.read t sha >|= function
     | Some Git.Value.Tree t -> Git.Tree.pp Fmt.stdout t;find_cache_libs t
     | Some a -> Git.Value.pp Fmt.stdout a;failwith "not a tree!"
     | None -> Git.Hash.pp Fmt.stdout sha; failwith "sha not found"

let read_file file ref =
  fs >>= fun t ->
  ref >>= fun h ->
    Search.find t h (`Commit (`Path file)) >>= function
    | None     -> failwith "file not found"
    | Some sha -> FS.read_exn t sha >>= function
                 | Git.Value.Blob b -> Lwt.return (Git.Blob.to_raw b)
                 | _ -> failwith "not a valid path"

let read_libs context ref (lib_list:string list)  =
  Lwt_list.map_p (fun l -> read_file [l] ref) lib_list >|=
    String.concat "\n"  >|=
    Str.split (Str.regexp "\n") >|=
    Lwt_stream.of_list >>=
    fun sl -> add_lib sl context

let process_file initctx schname content =
  let fileout = (String.sub schname 0 (String.length schname - 4)) ^ ".svg" in
  initctx >>= fun init ->
  content >|=
    Str.split (Str.regexp "\n") >>= fun lines ->
  Lwt_io.open_file Lwt_io.Output fileout >>=
    fun o -> Lwt_stream.fold parse_line (Lwt_stream.of_list lines) init >>=
    fun endcontext ->  output_context endcontext o >>=
     fun _ -> Lwt_io.close o

let rev_parse r =
  Lwt_process.pread ("", [|"git" ;"rev-parse"; r|]) >|= (fun s ->
    Git.Hash.of_raw (Str.first_chars s 40))

let () =
  let from_ref = rev_parse Sys.argv.(1) in
  let to_ref = rev_parse Sys.argv.(2) in
  let filename = Sys.argv.(3) in
  let from_context = find_libs from_ref >>= read_libs (initial_context ()) from_ref in
  let to_context = find_libs to_ref >>= read_libs (initial_context ()) to_ref in
  let from_content = read_file [filename] from_ref in
  let to_content = read_file [filename] to_ref in
  let first = from_ref >>= fun r -> process_file from_context ((Git.Hash.to_raw r) ^ "_" ^ filename) from_content in
  let second = to_ref >>= fun r -> process_file to_context ((Git.Hash.to_raw r) ^ "_" ^ filename) to_content in
  let total = Lwt.join [ first; second] in
   Lwt_main.run total

(*
 * utiliser une commande de compare pour comparer les fichiers
*)

    (* Lwt_io.open_file Lwt_io.Output (from_ref ^ filename) >>=
                 fun o ->from_context >|= fun c ->
               from_content >|=
                Str.split (Str.regexp "\n") >|=
                 fun lines -> Lwt_stream.fold parse_line( Lwt_stream.of_list lines) c >>= fun endcontext -> output_context endcontext o >>= fun _ -> Lwt_io.close o
               *)
