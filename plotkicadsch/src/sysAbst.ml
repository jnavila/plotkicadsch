type os =
  | MacOS
  | Linux
  | Windows
  | Cygwin

let process_output_to_string = fun command ->
  let chan = Unix.open_process_in command in
  let res = ref "" in
  let rec process_otl_aux () =
    let e = input_line chan in
    res := e ^ !res;
    process_otl_aux() in
  try process_otl_aux ()
  with End_of_file ->
    let stat = Unix.close_process_in chan in (!res,stat)
let cmd_output command =
  let (l,_) = process_output_to_string command in l

let launch_on_windows command =
  let (_,s) = process_output_to_string ("start " ^ command) in
  Lwt.return s

let detect_os () : os =
  if Sys.win32 then Windows else
    if Sys.cygwin then Cygwin else
      let in_ch,_,_ as uname = Unix.open_process_full "uname" [||] in
      let os = input_line in_ch in
      ignore (Unix.close_process_full uname);
      match os with
      | "Darwin" -> MacOS
      | "Linux" -> Linux
      | _ -> failwith "unknown operating system"

let windows_quote s =
  Re.(replace (Posix.compile_pat {|\^|&|\||\(|<|>|}) ~f:(fun ss -> "^" ^ (Group.get ss 0)) s)

let exec c a =
  match detect_os () with
  | MacOS | Linux -> Lwt_process.exec (c, Array.append [|c|]a)
  | Cygwin | Windows -> launch_on_windows @@ Array.fold_left (fun f g -> f ^ " " ^ (windows_quote g)) c a

let pread c a =
  match detect_os () with
  | MacOS | Linux -> Lwt_process.pread ~stderr:`Dev_null (c, (Array.append [|c|] a))
  | Cygwin | Windows ->
     Lwt.return @@ cmd_output (Array.fold_left (fun f g -> f ^ " " ^ (windows_quote g)) c a)
