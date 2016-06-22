open Kicadsch

let stream_map f stream =
  let next i =
    try Some (f (Stream.next stream))
    with Stream.Failure -> None in
  Stream.from next

let stream_context_map f initial_context stream =
  let context = ref initial_context in
  let next i =
    try
      let next_c, v = f !context (Stream.next stream) in
      (context := next_c;
      Some (v))
    with Stream.Failure -> None in
  Stream.from next

let stream_deoptionalize stream =
  let rec next i =
    try
      let o = Stream.next stream in
      match o with
      | Some _ as v -> v
      | None -> next i
    with Stream.Failure -> None in
  Stream.from next
    
let list_of_stream stream =
  let result = ref [] in
  Stream.iter (fun value -> result := value :: !result) stream;
  List.rev !result

open Svg
open Svg.M
open Svg_types.Unit
    
let _ =
  let ic = open_in "/home/jnavila/Developpement/kicad/kicad-library-utils/sch/complex_hierarchy/complex_hierarchy.sch" in
  let oc = open_out "../electric/complex.svg" in
  let ss () = Stream.from (fun _ -> try Some (input_line ic) with _ -> None) in
  let option_stream () = stream_context_map parse_line initial_context (ss ()) in
  let os () = stream_deoptionalize (option_stream ()) in
  let svg_doc = svg  ~a:[a_width (29.7, Some `Cm); a_height (21., Some `Cm); a_viewbox (0.,0., 11693., 8268.)] (list_of_stream (os ())) in
  Svg.P.print (fun s -> output_string oc s) svg_doc;
  close_out oc;
  close_in ic
  
