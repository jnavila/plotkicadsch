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

let stream_fold f stream init =
  let result = ref init in
  Stream.iter
    (fun x -> result := f x !result)
    stream;
  !result

let list_of_stream stream =
  let result = ref [] in
  Stream.iter (fun value -> result := value :: !result) stream;
  List.rev !result


let _ =
  let ic = open_in "/home/jnavila/Developpement/kicad/kicad-library-utils/sch/complex_hierarchy/complex_hierarchy.sch" in
  let oc = open_out "../electric/complex.svg" in
  let ss () = Stream.from (fun _ -> try Some (input_line ic) with _ -> None) in
  let endcontext = stream_fold parse_line  (ss ()) initial_context in
  output_context endcontext oc;
  close_out oc;
  close_in ic
