open Kicadsch
module SvgSchPainter = MakeSchPainter (SvgPainter)
open SvgSchPainter

let build_outputfilename outdir sch =
  let open Filename in
  let basefilename =
    if String.equal outdir "" then sch else concat outdir (basename sch)
  in
  remove_extension basefilename ^ ".svg"

let process_file init outdir sch =
  let%lwt initctx = init in
  let fileout = build_outputfilename outdir sch in
  let%lwt o = Lwt_io.open_file ~mode:Lwt_io.Output fileout in
  let%lwt i = Lwt_io.open_file ~mode:Lwt_io.Input sch in
  let%lwt contents = Lwt_io.read i in
  let endcontext = parse_sheet initctx contents in
  let canvas : SvgPainter.t = output_context endcontext in
  let%lwt () = Lwt_io.write o (SvgPainter.write canvas) in
  let%lwt () = Lwt_io.close i in
  Lwt_io.close o

let process_libs libs =
  Lwt_list.fold_left_s
    (fun c l ->
       let%lwt i = Lwt_io.open_file ~mode:Lwt_io.Input l in
       let%lwt content = Lwt_io.read i in
       Lwt.return (add_lib content c))
    (initial_context No_Rev) libs

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
  Lwt_main.run
    (let c = process_libs !libs in
     Lwt_list.iter_p (process_file c !outpath) !files)
