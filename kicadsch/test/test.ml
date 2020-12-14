open OUnit
open StdLabels

module MUT = Kicadsch.MakeSchPainter(StubPainter)
let initial_sheet = {|EESchema Schematic File Version 4
EELAYER 26 0
EELAYER END
|}

let initial_lib ={|DEF C C 0 10 N Y 1 F N
F0 "C" 25 100 50 H V L CNN
F1 "C" 25 -100 50 H V L CNN
F2 "" 38 -150 50 H I C CNN
F3 "" 0 0 50 H I C CNN
$FPLIST
 C_*
$ENDFPLIST
DRAW
ENDDRAW
ENDDEF
|}

let init () =
  let lib_lines =
    initial_lib
    |> String.split_on_char ~sep:'\n'
    |> List.fold_left ~f:(fun a b -> MUT.add_lib b a) ~init:(MUT.initial_context ()) in
  initial_sheet
  |> String.split_on_char ~sep:'\n'
  |> List.fold_left ~f:(fun a b -> MUT.parse_line b a) ~init:lib_lines
;;


let test_printable_F_line () =
  let u =
    {|
$Comp
L C C?
U 1 1 5FE7760D
P 3750 2500
F 0 "C?" H 3865 2546 50  0001 L CNN
F 1 "C" H 3865 2455 50  0000 L CNN
F 2 "" H 3788 2350 50  0001 C CNN
F 3 "~" H 3750 2500 50  0001 C CNN
	1    3750 2500
	1    0    0    -1
$EndComp
$EndSCHEMATC|}
    |> String.split_on_char ~sep:'\n'
    |> List.fold_left ~f:(fun a b -> MUT.parse_line b a) ~init:(init ()) in
  let output = StubPainter.write (MUT.output_context u) in
  match output with
  | [] -> assert_failure "Field should have been printed"
  | [v] ->  assert_equal  v "Text Black C Orient_H 3865 2545 50 J_left NoStyle"
  | u::v::w ->List.iter ~f:(Printf.printf "%s\n") output;assert_failure "Only one line should be printed\n"
;;

let test_escaped_F_line () =
  let u =
    {|
$Comp
L C C?
U 1 1 5FE7760D
P 3750 2500
F 0 "C?" H 3865 2546 50  0001 L CNN
F 1 "C\" 3" H 3865 2455 50  0000 L CNN
F 2 "" H 3788 2350 50  0001 C CNN
F 3 "~" H 3750 2500 50  0001 C CNN
	1    3750 2500
	1    0    0    -1
$EndComp
$EndSCHEMATC|}
    |> String.split_on_char ~sep:'\n'
    |> List.fold_left ~f:(fun a b -> MUT.parse_line b a) ~init:(init ()) in
  let output = StubPainter.write (MUT.output_context u) in
  match output with
  | [] -> assert_failure "Field should have been printed"
  | [v] -> assert_equal  v "Text Black C\" 3 Orient_H 3865 2545 50 J_left NoStyle"
  | u::v::w -> assert_failure "Only one line should be printed"
;;

let test_zero_length_lines () =
  let u =
    {|
$Comp
L C C?
U 1 1 5FE7760D
P 3750 2500
F 0 "C?" H 3865 2546 50  0001 L CNN
F 1 "~" H 3865 2455 50  0000 L CNN
F 2 "" H 3788 2350 50  0001 C CNN
F 3 "~" H 3750 2500 50  0001 C CNN
	1    3750 2500
	1    0    0    -1
$EndComp
$EndSCHEMATC|}
    |> String.split_on_char ~sep:'\n'
    |> List.fold_left ~f:(fun a b -> MUT.parse_line b a) ~init:(init ()) in
  let output = StubPainter.write (MUT.output_context u) in
  match output with
  | [] -> ()
  | _ -> assert_failure "Field should not have been printed"
;;

let match_wire_line () =
  let line = "	5500 1700 5500 2200" in
  let u = MUT.parse_line "Wire Wire Line" (init ())  in
  let v = MUT.parse_line line u in
  match StubPainter.write (MUT.output_context v) with
  | [v] -> ()
  | _ -> assert_failure "Wire line should have matched"
;;

let suite = "OUnit for " >::: [
    "printable F line" >:: test_printable_F_line;
    "match wire line" >:: match_wire_line;
    "zero length lines" >:: test_zero_length_lines;
    "escaped field lines" >:: test_escaped_F_line;
  ]
let _ =
  run_test_tt_main suite
