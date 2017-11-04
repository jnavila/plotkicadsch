open OUnit

module MUT = Kicadsch.MakeSchPainter(StubPainter)


let init () = MUT.initial_context ()

let test_printable_F_line () =
  let line = "F 0 \"Y1\" V 10004 3631 50  0000 L CNN" in
  let u = MUT.parse_line "$Comp" (init ())  in
  let v = MUT.parse_line line u in
  match StubPainter.write (MUT.output_context v) with
  | [] -> failwith "Field should have been printed"
  | [v] -> assert true
  | u::v::w -> failwith "Only one line should be printed"

let test_notprintable_F_line () =
  let line = "F 0 \"ED10\" H 1190 1270 40  0001 C CNN" in
  let u = MUT.parse_line "$Comp" (init ())  in
  let u' = MUT.parse_line line u in
  let v = MUT.parse_line "$EndComp" u' in
  match StubPainter.write (MUT.output_context v) with
  | [] -> ()
  | _ -> failwith "Field should not have been printed"

let match_wire_line () =
  let line = "	5500 1700 5500 2200" in
  let u = MUT.parse_line "Wire Wire Line" (init ())  in
  let v = MUT.parse_line line u in
  match StubPainter.write (MUT.output_context v) with
  | [v] -> ()
  | _ -> failwith "Wire line should have matched"

let suite = "OUnit for " >::: [
      (*  "test_printable_F_line" >:: test_printable_F_line; *)
  "test_notprintable_F_line" >:: test_notprintable_F_line;
  "match_wire_line" >:: match_wire_line
  ]
let _ =
  run_test_tt_main suite
