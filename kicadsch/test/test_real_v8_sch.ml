open OUnit
open StdLabels

module MUT = Kicadsch.V8.MakeSchPainter(StubPainter)

(* ── Helpers ─────────────────────────────────────────────────────────── *)

let real_sch_file = "Non-Isolated AC-DC Power Module.kicad_sch"

let read_file path =
  let ic = open_in path in
  let n  = in_channel_length ic in
  let s  = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

let init () = MUT.initial_context No_Rev

let parse_file () =
  let content = read_file real_sch_file in
  MUT.parse_sheet (init ()) content

let output_file () =
  StubPainter.write (MUT.output_context (parse_file ()))

let filter_tag tag out =
  List.filter ~f:(fun s ->
      let n = String.length tag + 1 in
      String.length s >= n &&
      String.sub s ~pos:0 ~len:n = (tag ^ " ")) out

let lines_of   = filter_tag "Line"
let rects_of   = filter_tag "Rect"
let texts_of   = filter_tag "Text"

(* ── Tests ───────────────────────────────────────────────────────────── *)

(* 1. The full real schematic parses without raising an exception. *)
let test_full_parse () =
  let _ = parse_file () in ()

(* 2. A known horizontal wire is present.
      (xy 150.114 85.344) -> (xy 165.354 85.344)
      wx_size x = int_of_float (x *. 100.0)
        150.114 * 100 = 15011.4 -> 15011
        165.354 * 100 = 16535.4 -> 16535
         85.344 * 100 =  8534.4 ->  8534
      Expected: "Line 15011 8534 - 16535 8534" *)
let test_known_wire () =
  let out = output_file () in
  assert_bool "Known horizontal wire Line 15011 8534 - 16535 8534 present"
    (List.mem "Line 15011 8534 - 16535 8534" ~set:(lines_of out))

(* 3. The large bounding rectangle is present.
      start=(35.306, 59.182)  end=(267.97, 132.842)
      rectangle_args uses rect_to_sheet_rec: paint_rect start (end - start)
        start = Coord(3530, 5918)
        dim   = Coord(26797-3530, 13284-5918) = Coord(23267, 7366)
      Expected: "Rect 3530 5918 23267 7366" *)
let test_bounding_rect () =
  let out = output_file () in
  assert_bool "Bounding rectangle Rect 3530 5918 23267 7366 present"
    (List.mem "Rect 3530 5918 23267 7366" ~set:(rects_of out))

(* 4. The "5V" net label is present with correct coordinates.
      (at 222.504 85.344 0) font=(1.27 1.27) justify=left
        222.504 * 100 = 22250.4 -> 22250
         85.344 * 100 =  8534.4 ->  8534
         1.27   * 100 =   127.0 ->   127
      Expected: "Text Red 5V Orient_H 22250 8534 127 J_left NoStyle" *)
let test_label_5v () =
  let out = output_file () in
  assert_bool "Label 5V present with correct coords"
    (List.mem "Text Red 5V Orient_H 22250 8534 127 J_left NoStyle"
       ~set:(texts_of out))

(* 5. The large italic title label is present.
      "Non-Isolated AC-DC Power Supply Design"
      (at 92.964 67.056 0) font=(4 4) italic justify=left
        92.964 * 100 = 9296.4 -> 9296
        67.056 * 100 = 6705.6 -> 6705  (wait: int_of_float truncates)
        Actually: int_of_float(67.056 *. 100.0) = int_of_float(6705.6) = 6705
        4.0    * 100 = 400
      Expected: "Text Red Non-Isolated AC-DC Power Supply Design Orient_H 9296 6705 400 J_left NoStyle" *)
let test_title_label () =
  let out = output_file () in
  assert_bool "Title label text present with correct coords and size"
    (List.mem
       "Text Red Non-Isolated AC-DC Power Supply Design Orient_H 9296 6705 400 J_left NoStyle"
       ~set:(texts_of out))

(* 6. At least one component reference text is present (R1 resistor).
      Property "Reference" "R1" at (152.4, 107.4419, 0), font=(1.27,1.27) justify=left
      draw_field is called with prop.at as origin, so xrel=yrel=0: drawn at raw position.
        x = wx_size(152.4) = 15240
        y = wx_size(107.4419) = 10744
      Expected: "Text Black R1 Orient_H 15240 10744 127 J_left NoStyle" *)
let test_component_reference () =
  let out = output_file () in
  assert_bool "Component reference R1 rendered at correct coords"
    (List.mem "Text Black R1 Orient_H 15240 10744 127 J_left NoStyle"
       ~set:(texts_of out))

(* 7. fields_autoplaced yes (bare atom form) in symbol does not crash the parse.
      Several symbols have (fields_autoplaced yes); full parse above covers this,
      but we make it explicit by checking output contains expected symbol output. *)
let test_fields_autoplaced_does_not_crash () =
  (* If fields_autoplaced caused a parse error the output_file call would raise. *)
  let out = output_file () in
  assert_bool "Output is non-empty when fields_autoplaced yes present"
    (out <> [])

(* 8. The zero-size degenerate rectangle is also emitted (start == end).
      start=(119.38 58.42) end=(119.38 58.42)
      rect_to_sheet_rec: start=Coord(11938, 5842), dim=Coord(0,0)
      Expected: "Rect 11938 5842 0 0" *)
let test_degenerate_rect () =
  let out = output_file () in
  assert_bool "Degenerate zero-size rectangle Rect 11938 5842 0 0 present"
    (List.mem "Rect 11938 5842 0 0" ~set:(rects_of out))

(* ── Suite ────────────────────────────────────────────────────────────── *)

let suite = "Real V8 schematic integration" >:::
  [ "full real schematic parses without exception"  >:: test_full_parse
  ; "known horizontal wire segment present"         >:: test_known_wire
  ; "bounding rectangle correct corners"            >:: test_bounding_rect
  ; "net label 5V correct coords and size"          >:: test_label_5v
  ; "title label correct text, coords, size"        >:: test_title_label
  ; "component reference R1 rendered correctly"     >:: test_component_reference
  ; "fields_autoplaced yes does not crash"          >:: test_fields_autoplaced_does_not_crash
  ; "degenerate zero-size rectangle present"        >:: test_degenerate_rect
  ]

let _ = run_test_tt_main suite
