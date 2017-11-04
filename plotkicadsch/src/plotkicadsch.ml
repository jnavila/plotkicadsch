open Kicadsch
module SvgSchPainter = MakeSchPainter(SvgPainter)
open Lwt
open SvgSchPainter

let process_file init sch =
  let slen = String.length sch in
  let fileout = (String.sub sch 0 (slen - 4)) ^ ".svg" in
  Lwt_io.open_file ~mode:Lwt_io.Output fileout >>=
    fun o -> Lwt_io.open_file ~mode:Lwt_io.Input sch >>=
    fun i -> Lwt_stream.fold parse_line (Lwt_io.read_lines i) init >>=
    fun endcontext -> let canvas:SvgPainter.t = (output_context endcontext) in
    Lwt_io.write o (SvgPainter.write canvas) >>=
    fun _ -> Lwt_io.close i >>=
    fun _ -> Lwt_io.close o

let process_libs libs =
  Lwt_list.fold_left_s (fun c l -> Lwt_stream.fold add_lib (Lwt_io.lines_of_file l) c) (initial_context () ) libs

let () =
  let files = ref [] in
  let libs = ref [] in
  let speclist = [
      ("-l", Arg.String (fun lib -> libs := lib::!libs), "specify component library");
      ("-f", Arg.String(fun sch -> files := sch::!files), "sch file to process")] in
  let usage_msg = "plotkicadsch prints Kicad sch files to svg" in
  Arg.parse speclist print_endline usage_msg;
  Lwt_main.run
    begin
      process_libs !libs >>=
        fun c -> Lwt_list.iter_p (process_file c) !files
    end
