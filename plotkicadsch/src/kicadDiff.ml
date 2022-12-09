open! StdLabels
open Lwt.Infix
open Kicadsch.Sigs
include DiffFs
open DiffTool

let doc = function
  | TrueFS s -> "file system " ^ s
  | GitFS s -> "Git rev " ^ s

let git_fs commitish = GitFS commitish
let true_fs rootname = TrueFS rootname

type differ = Internal of string | Image_Diff

let fs_mod s r =
  let rel_path = Option.bind r (fun rel_path ->
      if (String.length rel_path > 1) &&
           String.equal (String.sub rel_path ~pos:0 ~len:2) "./" then
        begin
          if String.length rel_path == 2 then
            None
          else
            Some (String.sub rel_path ~pos:2 ~len:(String.length rel_path - 2))
        end
      else
        Some rel_path) in
  match s with
  | GitFS s -> GitFs.make s rel_path
  | TrueFS s -> TrueFs.make s rel_path

let is_suffix ~suffix s =
  let suff_length = String.length suffix in
  let s_length = String.length s in
  (suff_length < s_length) &&
  (String.equal (String.sub s ~pos:(String.length s - suff_length) ~len:suff_length) suffix)
;;

module FSPainter (S : SchPainter) (F : Simple_FS) : sig
  val find_schematics : unit -> (string list * string) list Lwt.t

  val process_file : S.schContext Lwt.t -> string list -> S.painterContext Lwt.t

  val context_from : S.schContext Lwt.t -> S.schContext Lwt.t
end = struct

  let find_schematics () = F.list_files (is_suffix ~suffix:".sch")

  let process_file initctx filename =
    let%lwt init = initctx in
    F.get_content filename
    >|= S.parse_sheet init >|= S.output_context

  let find_libs () =
    F.list_files (is_suffix ~suffix:"-cache.lib") >|= List.map ~f:fst

  let read_libs (initial_ctx:S.schContext) (lib_list: string list list) : S.schContext Lwt.t =
    Lwt_list.fold_left_s
      (fun c l ->
         let%lwt content = F.get_content l in
         Lwt.return (S.add_lib content c))
      initial_ctx lib_list

  let context_from from_ctx : S.schContext Lwt.t =
    let%lwt initial_context = from_ctx in
    find_libs () >>= read_libs initial_context
end

module PathCompare = struct
  type t = string list * string

  let rec sl_compare l1 l2 =
    match l1, l2 with
    | name1::tl1, name2::tl2 -> let res = String.compare name1 name2 in
      if res == 0 then
        sl_compare tl1 tl2
      else
        res
    | _h::_t, [] -> 1
    | [], _h::_t -> -1
    | [], [] -> 0
  let compare (l1, _) (l2, _) = sl_compare l1 l2

end
module PathSet = Set.Make(PathCompare)

let merge_lists l1l l2l =
  l1l
  >>= fun l1 ->
  l2l
  >|= fun l2 ->
  let r = PathSet.empty in
  let r1 = List.fold_left ~f:(fun s l -> PathSet.add l s) ~init:r l1 in
  let r2 = List.fold_left ~f:(fun s l -> PathSet.add l s) ~init:r1 l2 in
  PathSet.elements r2 |> List.rev_map ~f:fst

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

let doit from_fs to_fs file_to_diff differ textdiff libs keep colors zone_color allow_missing_component relative_path =
  let module_d =
    match differ with
    | Image_Diff ->
      (module ImageDiff : Differ)
    | Internal s ->
      InternalDiff.internal_diff s colors zone_color
  in
  let module D = (val module_d : Differ) in
  let module F = (val (fs_mod from_fs relative_path) : Simple_FS) in
  let module T = (val (fs_mod to_fs relative_path) : Simple_FS) in
  let module FromP = FSPainter (D.S) (F) in
  let module ToP = FSPainter (D.S) (T) in
  let file_list =
    match file_to_diff with
    | None ->
      let from_list = FromP.find_schematics () in
      let to_list = ToP.find_schematics () in
      merge_lists from_list to_list
    | Some filename ->
      let filename_l = String.split_on_char ~sep:'/' filename in
      Lwt.return [filename_l]
  in
  let preload_libs desc =
    Lwt_list.fold_left_s
      (fun c f -> Lwt_stream.fold D.S.add_lib (Lwt_io.lines_of_file f) c)
      (D.S.initial_context ~allow_missing_component desc) libs
  in
  let from_init_ctx = FromP.context_from @@ preload_libs (First (doc from_fs)) in
  let to_init_ctx = ToP.context_from @@ preload_libs (Second (doc to_fs)) in
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
  let compare_all = file_list >>= Lwt_list.iter_p compare_one in
  let catch_errors =
    Lwt.catch
      (fun _ ->
         Lwt_io.printf "%s between %s and %s\n" D.doc (doc F.label) (doc T.label)
         >>= fun _ -> compare_all )
      (function
        | GitFs.InternalGitError s ->
          Lwt_io.printf "Git Exception: %s\n" s
        | a ->
          Lwt_io.printf "Exception %s\n" (Printexc.to_string a) )
  in
  Lwt_main.run catch_errors
