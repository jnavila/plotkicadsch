open  Kicadsch.Sigs

let build_outputfilename outdir sch =
  let open Filename in
  let basefilename =
    if String.equal outdir "" then sch else concat outdir (basename sch)
  in
  remove_extension basefilename ^ ".svg"

let process_files lib_files sch_files outdir =
  let sch = List.hd sch_files in
  let schHandler =
    if String.ends_with ~suffix:".sch" sch then
      (module Kicadsch.V5: Kicadsch.Sigs.KicadSchHandler)
    else if String.ends_with ~suffix:".kicad_sch" sch then
      (module Kicadsch.V6: Kicadsch.Sigs.KicadSchHandler)
    else
      failwith ("unknown file extenstion for " ^ sch) in
  let module SchPainter = (val schHandler) in
  let module SvgSchModule = SchPainter.MakeSchPainter(SvgPainter) in
  let open SvgSchModule in
  let initctx = initial_context No_Rev in
  let%lwt init = Lwt_list.fold_left_s (fun context lib ->
      let%lwt i = Lwt_io.open_file ~mode:Lwt_io.Input lib in
      let%lwt content = Lwt_io.read i in
      Lwt.return (add_lib content context) ) initctx lib_files in
  let process_file sch_file =
    let fileout = build_outputfilename outdir sch_file in
    let%lwt o = Lwt_io.open_file ~mode:Lwt_io.Output fileout in
    let%lwt i = Lwt_io.open_file ~mode:Lwt_io.Input sch in
    let%lwt contents = Lwt_io.read i in
    let endcontext = parse_sheet init contents in
    let canvas = output_context endcontext in
    let%lwt () = Lwt_io.write o (SvgPainter.write canvas) in
    let%lwt () = Lwt_io.close i in
    Lwt_io.close o in
    Lwt_list.iter_p process_file sch_files


let () =
  let files = ref [] in
  let libs = ref [] in
  let outpath = ref "" in
  let speclist =
    [ ( "-l"
      , Arg.String (fun lib -> libs := lib :: !libs)
      , "specify component library" )
    ; ( "-f"
      , Arg.String (fun sch -> files := sch :: !files)
      , "sch file to process" )
    ; ( "-o"
      , Arg.String (fun o -> outpath := o)
      , "full path of output directory" ) ]
  in
  let usage_msg = "plotkicadsch prints Kicad sch files to svg" in
  Arg.parse speclist print_endline usage_msg ;
  Lwt_main.run (process_files !libs !files !outpath)
