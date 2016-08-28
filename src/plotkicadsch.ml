module SvgSchPainter = Kicadsch.MakeSchPainter(SvgPainter)
open SvgSchPainter

let stream_fold f stream init =
  let result = ref init in
  Stream.iter
    (fun x -> result := f x !result)
    stream;
  !result

let () =
  let init = ref (initial_context ()) in
  let speclist = [
      ("-l", Arg.String (fun lib -> init := add_lib lib !init ), "specify component library");
      ("-f", Arg.String(fun sch ->
               let fileout = sch ^ ".svg" in
               let ic = open_in sch in
               let oc = open_out fileout in
               let ss () = Stream.from (fun _ -> try Some (input_line ic) with _ -> None) in
               let endcontext = stream_fold parse_line  (ss ()) !init in
               output_context endcontext oc;
               Printf.printf "processing %s to %s\n" sch fileout;
               close_out oc;
               close_in ic), "sch file to process")] in
  let usage_msg = "plotkicadsch prints Kicad sch files to svg" in
  Arg.parse speclist print_endline usage_msg
