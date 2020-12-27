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

let segment_horizontal_wire wire_type () =
  let u =
    Printf.sprintf {|Wire %s Line
	5500 1700 5500 2200
Connection ~ 5500 1800
Entry Wire Line
	5500 2000 5550 2050
Entry Bus Line
	5500 2100 5550 2150 5550
|} wire_type

    |> String.split_on_char ~sep:'\n'
    |> List.fold_left ~f:(fun a b -> MUT.parse_line b a) ~init:(init ()) in
  let output = StubPainter.write (MUT.output_context u) in
  assert_bool "Connection segment present" (List.mem "Line 5500 1700 - 5500 1800" ~set:output);
  assert_bool "Entry wire segment present" (List.mem "Line 5500 1800 - 5500 2000" ~set:output);
  assert_bool "Entry bus segment present" (List.mem "Line 5500 2000 - 5500 2100" ~set:output);
  assert_bool "Fourth segment present" (List.mem "Line 5500 2100 - 5500 2200" ~set:output)

let segment_inverse_horizontal_wire wire_type () =
  let u =
    Printf.sprintf {|Wire %s Line
	5500 2200 5500 1700
Connection ~ 5500 1800
Entry Wire Line
	5500 2000 5550 2050
Entry Bus Line
	5500 2100 5550 2150
|} wire_type
    |> String.split_on_char ~sep:'\n'
    |> List.fold_left ~f:(fun a b -> MUT.parse_line b a) ~init:(init ()) in
  let output = StubPainter.write (MUT.output_context u) in
  assert_bool "Connection segment present" (List.mem "Line 5500 1700 - 5500 1800" ~set:output);
  assert_bool "Entry wire segment present" (List.mem "Line 5500 1800 - 5500 2000" ~set:output);
  assert_bool "Entry bus segment present" (List.mem "Line 5500 2000 - 5500 2100" ~set:output);
  assert_bool "Fourth segment present" (List.mem "Line 5500 2100 - 5500 2200" ~set:output)
;;

let segment_vertical_wire wire_type () =
  let u =
    Printf.sprintf {|Wire %s Line
	 1700 5500 2200 5500
Connection ~ 1800 5500
Entry Wire Line
	2000 5500 2050 5550
Entry Bus Line
	2100 5500 2150 5550
|} wire_type
    |> String.split_on_char ~sep:'\n'
    |> List.fold_left ~f:(fun a b -> MUT.parse_line b a) ~init:(init ()) in
  let output = StubPainter.write (MUT.output_context u) in
  assert_bool "Connection segment present"  (List.mem "Line 1700 5500 - 1800 5500" ~set:output);
  assert_bool "Entry wire segment present" (List.mem "Line 1800 5500 - 2000 5500" ~set:output);
  assert_bool "Entry bus segment present"  (List.mem "Line 2000 5500 - 2100 5500" ~set:output);
  assert_bool "Fourth segment present" (List.mem "Line 2100 5500 - 2200 5500" ~set:output)
;;

let segment_inverse_vertical_wire wire_type () =
  let u =
     Printf.sprintf {|Wire %s Line
	 2200 5500 1700 5500
Connection ~ 1800 5500
Entry Wire Line
	2000 5500 2050 5550
Entry Bus Line
	2100 5500 2150 5550
|} wire_type
    |> String.split_on_char ~sep:'\n'
    |> List.fold_left ~f:(fun a b -> MUT.parse_line b a) ~init:(init ()) in
  let output = StubPainter.write (MUT.output_context u) in
  assert_bool "Connection segment present"  (List.mem "Line 1700 5500 - 1800 5500" ~set:output);
  assert_bool "Entry wire segment present" (List.mem "Line 1800 5500 - 2000 5500" ~set:output);
  assert_bool "Entry bus segment present"  (List.mem "Line 2000 5500 - 2100 5500" ~set:output);
  assert_bool "Fourth segment present" (List.mem "Line 2100 5500 - 2200 5500" ~set:output)
;;

let segment_vertical_wire_test () =
  let u =
      {|Wire Wire Line
	6000 1150 6000 2750
Wire Wire Line
	6000 1350 6000 1350
Connection ~ 6000 1350
Connection ~ 6000 2050
Connection ~ 6000 1250
Connection ~ 6000 1150
|}
    |> String.split_on_char ~sep:'\n'
    |> List.fold_left ~f:(fun a b -> MUT.parse_line b a) ~init:(init ()) in
  let output = StubPainter.write (MUT.output_context u) in
  assert_bool "Wire 1150 - 1250"  (List.mem "Line 6000 1150 - 6000 1250" ~set:output)
  ; assert_bool "Wire 1250 - 1350" (List.mem "Line 6000 1250 - 6000 1350" ~set:output)
  ; assert_bool "Wire 1350 - 2050"  (List.mem "Line 6000 1350 - 6000 2050" ~set:output)
  ; assert_bool "Wire 2050 - 2750" (List.mem "Line 6000 2050 - 6000 2750" ~set:output)
  ; assert_bool "no Wire 1150 - 1150" (not(List.mem "Line 6000 1150 - 6000 1150" ~set:output))
;;

let suite = "OUnit for " >:::
            [ "printable F line" >:: test_printable_F_line
            ; "match wire line" >:: match_wire_line
            ; "zero length lines" >:: test_zero_length_lines
            ; "escaped field lines" >:: test_escaped_F_line
            ; "Segment horizontal wire" >:: segment_horizontal_wire "Wire"
            ; "Segment inverse horizontal wire" >:: segment_inverse_horizontal_wire "Wire"
            ; "Segment vertical wire" >:: segment_vertical_wire "Wire"
            ; "Segment inverse vertical wire" >:: segment_inverse_vertical_wire "Wire"
            ; "Segment horizontal bus" >:: segment_horizontal_wire "Bus"
            ; "Segment inverse horizontal bus" >:: segment_inverse_horizontal_wire "Bus"
            ; "Segment vertical bus" >:: segment_vertical_wire "Bus"
            ; "Segment inverse vertical bus" >:: segment_inverse_vertical_wire "Bus"
            ; "Segment vertical test ">:: segment_vertical_wire_test
            ]
let _ =
  run_test_tt_main suite
