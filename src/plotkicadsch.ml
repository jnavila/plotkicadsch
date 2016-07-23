module SvgSchPainter = Kicadsch.MakeSchPainter(SvgPainter)
open SvgSchPainter

let stream_fold f stream init =
  let result = ref init in
  Stream.iter
    (fun x -> result := f x !result)
    stream;
  !result

let _ =
  let init = initial_context () in
  let lib = "/home/jnavila/Developpement/kicad/kicad-library-utils/sch/complex_hierarchy/complex_hierarchy-cache.lib" in
  let with_lib = add_lib lib init in
  let ic = open_in "/home/jnavila/Developpement/kicad/kicad-library-utils/sch/complex_hierarchy/complex_hierarchy.sch" in
  let oc = open_out "../electric/complex.svg" in
  let ss () = Stream.from (fun _ -> try Some (input_line ic) with _ -> None) in
  let endcontext = stream_fold parse_line  (ss ()) with_lib in
  output_context endcontext oc;
  close_out oc;
  close_in ic
