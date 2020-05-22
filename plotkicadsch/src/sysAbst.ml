open Core_kernel

type os = MacOS | Linux | Windows | Cygwin

let process_output_to_string command =
  let chan = Unix.open_process_in command in
  let res = ref "" in
  let rec process_otl_aux () =
    let e = input_line chan in
    res := e ^ !res ;
    process_otl_aux ()
  in
  try process_otl_aux ()
  with End_of_file ->
    let stat = Unix.close_process_in chan in
    (!res, stat)

let cmd_output command =
  let l, _ = process_output_to_string command in
  l

let launch_on_windows command =
  let _, s = process_output_to_string ("start " ^ command) in
  Lwt.return s

let detect_os () : os =
  if Sys.win32 then Windows
  else if Sys.cygwin then Cygwin
  else
    let ((in_ch, _, _) as uname) = Unix.open_process_full "uname" [||] in
    let os = input_line in_ch in
    ignore (Unix.close_process_full uname) ;
    match os with
    | "Darwin" ->
        MacOS
    | "Linux" ->
        Linux
    | _ ->
        failwith "unknown operating system"

let windows_quote s =
  Re.(
    replace
      (Posix.compile_pat {|\^|&|\||\(|<|>|})
      ~f:(fun ss -> "^" ^ Group.get ss 0)
      s)

let exec c a =
  match detect_os () with
  | MacOS | Linux ->
      Lwt_process.exec (c, Array.append [|c|] a)
  | Cygwin | Windows ->
      launch_on_windows
      @@ Array.fold ~f:(fun f g -> f ^ " " ^ windows_quote g) ~init:c a

let pread c a =
  match detect_os () with
  | MacOS | Linux ->
      Lwt_process.pread ~stderr:`Dev_null (c, Array.append [|c|] a)
  | Cygwin | Windows ->
      Lwt.return
      @@ cmd_output
           (Array.fold ~f:(fun f g -> f ^ " " ^ windows_quote g) ~init:c a)

let build_tmp_svg_name ~keep aprefix aschpath =
  let aschname = List.last_exn aschpath in
  let root_prefix =
    aprefix ^ String.sub aschname ~pos:0 ~len:(String.length aschname - 4)
  in
  match detect_os () with
  | MacOS | Linux ->
      Stdlib.Filename.temp_file root_prefix ".svg"
  | Cygwin | Windows ->
      if keep then root_prefix ^ ".svg"
      else Stdlib.Filename.temp_file root_prefix ".svg"

let finalize_tmp_file fnl ~keep_as =
  match detect_os () with
  | MacOS | Linux -> (
      try%lwt
        match keep_as with
        | None ->
            Lwt_unix.unlink fnl
        | Some target ->
            Lwt_unix.rename fnl target
      with _ -> Lwt.return_unit )
  | Cygwin | Windows ->
      Lwt.return_unit

let default_opener () =
  match detect_os () with
  | Linux ->
      "xdg-open"
  | MacOS ->
      "open"
  | Cygwin | Windows ->
      ""

(* we already use "start" in exec *)
