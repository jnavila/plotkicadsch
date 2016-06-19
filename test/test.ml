open OUnit

let test_printable_F_line () =
  let line = "F 0 \"ED10\" H 1190 1270 40  0000 C CNN" in
  let u = Kicadsch.parse_F line in
  match u with
  | None -> failwith "Field should have been printed"
  | Some v -> assert true

let test_notprintable_F_line () =
  let line = "F 0 \"ED10\" H 1190 1270 40  0001 C CNN" in
  let u = Kicadsch.parse_F line in
  match u with
  | None -> ()
  | Some v -> failwith "Field should not have been printed"

let match_wire_line () =
  let line = "	5500 1700 5500 2200" in
  let u = Kicadsch.parse_wire_line line in
  match u with
  | None -> failwith "Wire line should have matched"
  | Some l -> ()
     
let suite = "OUnit for " >::: [
  "test_printable_F_line" >:: test_printable_F_line;
  "test_notprintable_F_line" >:: test_notprintable_F_line;
  "match_wire_line" >:: match_wire_line
  ]
let _ =
  run_test_tt_main suite
