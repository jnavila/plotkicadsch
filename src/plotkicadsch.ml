module SvgSchPainter = Kicadsch.MakeSchPainter(SvgPainter)
open SvgSchPainter
open Lwt

let stream_fold f stream init =
  let result = ref init in
  Stream.iter
    (fun x -> result := f x !result)
    stream;
  !result

let process_file init sch =
  let slen = String.length sch in
  let fileout = (String.sub sch 0 (slen - 4)) ^ ".svg" in
  Lwt_io.open_file Lwt_io.Output fileout >>=
    fun o -> Lwt_io.open_file Lwt_io.Input sch >>=
    fun i -> Lwt_stream.fold parse_line (Lwt_io.read_lines i) init >>=
    fun endcontext ->  output_context endcontext o >>=
    fun _ -> Lwt_io.close i >>=
    fun _ -> Lwt_io.close o


let () =
  let init = ref @@ initial_context () in
  let files = ref [] in
  let speclist = [
      ("-l", Arg.String (fun lib ->Printf.printf "parsing lib %s\n" lib;
                                let ic = open_in lib in
                                init := add_lib ic !init ), "specify component library");
      ("-f", Arg.String(fun sch ->
                 files := sch::!files
               ), "sch file to process")] in
  let usage_msg = "plotkicadsch prints Kicad sch files to svg" in
  Arg.parse speclist print_endline usage_msg;
  Lwt_main.run (Lwt_list.iter_p (process_file !init) !files)
