open OUnit
open StdLabels

module MUT = Kicadsch.V8.MakeSchPainter(StubPainter)

(* ── Helpers ─────────────────────────────────────────────────────────── *)

(** Wrap a fragment in a minimal V8 schematic header (with A4 paper). *)
let wrap body = Printf.sprintf {|(kicad_sch
  (version 20250114)
  (generator "eeschema")
  (generator_version "9.0")
  (uuid "00000000-0000-0000-0000-000000000000")
  (paper "A4")
  (lib_symbols)
  %s
)|} body

let init () = MUT.initial_context No_Rev

let parse body = MUT.parse_sheet (init ()) (wrap body)
let output body = StubPainter.write (MUT.output_context (parse body))

let starts_with ~prefix s =
  let n = String.length prefix in
  String.length s >= n && String.sub s ~pos:0 ~len:n = prefix

let ends_with ~suffix s =
  let n = String.length suffix in
  let slen = String.length s in
  slen >= n && String.sub s ~pos:(slen - n) ~len:n = suffix

(** Keep only entries that start with the given tag word. *)
let filter_tag tag out =
  List.filter ~f:(starts_with ~prefix:(tag ^ " ")) out

let lines_of   = filter_tag "Line"
let circles_of = filter_tag "Circle"
let arcs_of    = filter_tag "Arc"
let rects_of   = filter_tag "Rect"
let texts_of   = filter_tag "Text"

(* ── Smoke tests ─────────────────────────────────────────────────────── *)

let test_minimal_parses () =
  let _ = MUT.parse_sheet (init ()) {|(kicad_sch
  (version 20250114)
  (generator "eeschema")
  (generator_version "9.0")
  (uuid "11111111-1111-1111-1111-111111111111")
  (paper "A4")
  (lib_symbols)
)|} in ()

let test_no_title_block_parses () =
  (* title_block with no children: all fields default to "" — no text drawn *)
  let out = output "(title_block)" in
  assert_equal ~msg:"empty title_block produces no text" [] (texts_of out)

let test_partial_title_block_parses () =
  (* only title and rev are present *)
  let out = output {|(title_block (title "My Board") (rev "B"))|} in
  let titles = List.filter ~f:(fun s ->
      String.length s > 12 &&
      String.sub s ~pos:0 ~len:12 = "Text Black T") (texts_of out) in
  assert_bool "Title text appears" (titles <> [])

(* ── Wire ────────────────────────────────────────────────────────────── *)

(* Wire segments are collected and then cut/painted in output_context.
   Internal units: wx_size x = int_of_float (x *. 100.)
   So (xy 0 0) → Coord(0,0)  and  (xy 10 0) → Coord(1000,0). *)
let test_wire_value () =
  let out = output {|(wire
    (pts (xy 0 0) (xy 10 0))
    (stroke (width 0) (type default))
    (uuid "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa")
  )|} in
  assert_bool "Wire segment Line 0 0 - 1000 0"
    (List.mem "Line 0 0 - 1000 0" ~set:(lines_of out))

(* ── Arc ─────────────────────────────────────────────────────────────── *)

(* Three points on the circle center=(1.0,2.0) radius=5.0 (file units).
     start=(6,2)  mid=(1,7)  end=(-4,2)
   Internal units after wx_size x100:
     start=(600,200)  mid=(100,700)  end=(-400,200)
   Circumcircle formula:
     d   = 2*((bx-ax)*(cy-by) - (cx-bx)*(by-ay)) = 1_000_000
     ab2 = 100_000    bc2 = -300_000
     ux  = 100        uy  = 200     r = 500
   Expected: "Arc 100 200 600 200 -400 200 500" *)
let test_arc_value () =
  let out = output {|(arc
    (start 6 2)
    (mid 1 7)
    (end -4 2)
    (stroke (width 0) (type default))
    (uuid "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaab")
  )|} in
  assert_bool "Arc drawn with correct center/start/end/radius"
    (List.mem "Arc 100 200 600 200 -400 200 500" ~set:(arcs_of out))

(* Collinear arc points → degenerate, center computation returns None → no arc drawn *)
let test_arc_collinear_no_output () =
  let out = output {|(arc
    (start 0 0)
    (mid 5 0)
    (end 10 0)
    (stroke (width 0) (type default))
    (uuid "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaac")
  )|} in
  assert_equal ~msg:"Degenerate arc produces no Arc entry" [] (arcs_of out)

(* ── Circle ──────────────────────────────────────────────────────────── *)

(* center=(1.0,2.0) radius=3.0 (file units)
   Internal: center=Coord(100,200)  radius = round(3.0*100) = 300
   Expected: "Circle 100 200 300" *)
let test_circle_value () =
  let out = output {|(circle
    (center 1 2)
    (radius 3)
    (stroke (width 0) (type default))
    (uuid "bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb")
  )|} in
  assert_bool "Circle drawn with correct center and radius"
    (List.mem "Circle 100 200 300" ~set:(circles_of out))

(* ── Bézier ──────────────────────────────────────────────────────────── *)

(* Cubic Bézier P0=(0,0) P1=(0,10) P2=(10,10) P3=(10,0) (file units).
   Internal: P0=(0,0)  P3=(1000,0).
   sch_bezier_sample produces 17 points → 16 line segments.
   B(0)=P0=(0,0) is the exact start;  B(1)=P3=(1000,0) is the exact end.
   draw_line accumulates with fold_left, so the output list is in reverse
   order of painting: head = last segment, last element = first segment.
   First segment: "Line 0 0 - <B1>"   starts with "Line 0 0 -"
   Last  segment: "Line <B15> - 1000 0"  ends with "- 1000 0" *)
let test_bezier_16_segments () =
  let out = output {|(bezier
    (pts (xy 0 0) (xy 0 10) (xy 10 10) (xy 10 0))
    (stroke (width 0) (type default))
    (uuid "cccccccc-cccc-cccc-cccc-cccccccccccc")
  )|} in
  let segs = lines_of out in
  assert_equal ~msg:"Bézier produces 16 line segments" 16 (List.length segs)

let test_bezier_starts_at_p0 () =
  let out = output {|(bezier
    (pts (xy 0 0) (xy 0 10) (xy 10 10) (xy 10 0))
    (stroke (width 0) (type default))
    (uuid "cccccccc-cccc-cccc-cccc-cccccccccccd")
  )|} in
  let segs = lines_of out in
  (* last element = first painted = first segment starting at P0=(0,0) *)
  let first_seg = List.nth segs (List.length segs - 1) in
  assert_bool "First segment starts at P0=(0,0)"
    (starts_with ~prefix:"Line 0 0 -" first_seg)

let test_bezier_ends_at_p3 () =
  let out = output {|(bezier
    (pts (xy 0 0) (xy 0 10) (xy 10 10) (xy 10 0))
    (stroke (width 0) (type default))
    (uuid "cccccccc-cccc-cccc-cccc-cccccccccccf")
  )|} in
  let segs = lines_of out in
  (* head = last painted = last segment ending at P3=(1000,0) *)
  let last_seg = List.hd segs in
  assert_bool "Last segment ends at P3=(1000,0)"
    (ends_with ~suffix:"- 1000 0" last_seg)

(* ── Rule area ───────────────────────────────────────────────────────── *)

(* Closed square (file units): (0,0)→(10,0)→(10,10)→(0,10)→(0,0)
   Internal: 4 edges of 1000 internal units each.
   Expected line segments (draw_line, kolor=Black):
     "Line 0 0 - 1000 0"       first edge
     "Line 1000 0 - 1000 1000" second edge
     "Line 1000 1000 - 0 1000" third edge
     "Line 0 1000 - 0 0"       fourth (closing) edge *)
let test_rule_area_value () =
  let out = output {|(rule_area
    (exclude_from_sim no)
    (in_bom no)
    (on_board no)
    (dnp no)
    (polyline
      (pts (xy 0 0) (xy 10 0) (xy 10 10) (xy 0 10) (xy 0 0))
      (stroke (width 0) (type default))
      (uuid "dddddddd-dddd-dddd-dddd-dddddddddddd")
    )
  )|} in
  let segs = lines_of out in
  assert_bool "First edge (0,0)→(1000,0)"
    (List.mem "Line 0 0 - 1000 0" ~set:segs);
  assert_bool "Second edge (1000,0)→(1000,1000)"
    (List.mem "Line 1000 0 - 1000 1000" ~set:segs);
  assert_bool "Third edge (1000,1000)→(0,1000)"
    (List.mem "Line 1000 1000 - 0 1000" ~set:segs);
  assert_bool "Fourth (closing) edge (0,1000)→(0,0)"
    (List.mem "Line 0 1000 - 0 0" ~set:segs)

(* ── Text box ────────────────────────────────────────────────────────── *)

(* text="Hello"  at=(1,2,0)  size=(5,3)  font=(1.27,1.27) justify=left
   Internal coordinates:
     corner = Coord(wx_size 1, wx_size 2) = Coord(100, 200)
     dim    = Coord(wx_size 5, wx_size 3) = Coord(500, 300)
     bottom_right = Coord(100+500, 200+300) = Coord(600, 500)
   paint_rect Coord(100,200) Coord(600,500) → "Rect 100 200 600 500"
   Font size = Coord(wx_size 1.27, wx_size 1.27) = Coord(127,127) → Size 127
   draw_text_line label kolor=Green (TextNote), orient=Orient_H (J_left)
   paint_text kolor=Green "Hello" Orient_H Coord(100,200) Size(127) J_left NoStyle
   → "Text Green Hello Orient_H 100 200 127 J_left NoStyle" *)
let test_text_box_at_size_value () =
  let out = output {|(text_box "Hello"
    (exclude_from_sim no)
    (at 1 2 0)
    (size 5 3)
    (stroke (width 0) (type default))
    (fill (type none))
    (effects (font (size 1.27 1.27)) (justify left))
    (uuid "eeeeeeee-eeee-eeee-eeee-eeeeeeeeeeee")
  )|} in
  assert_bool "Text content matches input string"
    (List.mem "Text Green Hello Orient_H 100 200 127 J_left NoStyle"
       ~set:(texts_of out));
  assert_bool "Bounding rectangle drawn with correct corners"
    (List.mem "Rect 100 200 600 500" ~set:(rects_of out))

(* Legacy start+end form produces the same corner and bottom-right *)
let test_text_box_start_end_value () =
  let out = output {|(text_box "World"
    (exclude_from_sim no)
    (start 1 2)
    (end 6 5)
    (stroke (width 0) (type default))
    (fill (type none))
    (effects (font (size 1.27 1.27)) (justify left))
    (uuid "eeeeeeee-eeee-eeee-eeee-eeeeeeeeeef0")
  )|} in
  (* start=(1,2)→Coord(100,200)  end=(6,5)→Coord(600,500)
     corner=Coord(100,200)  dim=Coord(600-100,500-200)=Coord(500,300)
     bottom-right = Coord(100+500,200+300) = Coord(600,500)
     → same rectangle as at+size form *)
  assert_bool "Text content from start/end form"
    (List.mem "Text Green World Orient_H 100 200 127 J_left NoStyle"
       ~set:(texts_of out));
  assert_bool "Rect from start/end form"
    (List.mem "Rect 100 200 600 500" ~set:(rects_of out))

(* ── Labels ──────────────────────────────────────────────────────────── *)

(* label "NET1" at=(0,0) justify=left → drawn as WireLabel (kolor=Red) *)
let test_label_value () =
  let out = output {|(label "NET1"
    (at 0 0 0)
    (effects (font (size 1.27 1.27)) (justify left))
    (uuid "ffffffff-ffff-ffff-ffff-ffffffffffff")
  )|} in
  assert_bool "Label text appears with correct coords and colour"
    (List.mem "Text Red NET1 Orient_H 0 0 127 J_left NoStyle"
       ~set:(texts_of out))

(* ── Metadata-only items: parse and produce no drawn output ───────────── *)

let test_ellipse_no_output () =
  let out = output {|(ellipse
    (center 100 100) (major_radius 50) (minor_radius 30) (rotation_angle 0)
    (stroke (width 0) (type default))
    (uuid "ffffffff-ffff-ffff-ffff-fffffffffffe")
  )|} in
  assert_equal ~msg:"ellipse produces no Arc/Circle/Line/Rect/Text" []
    (arcs_of out @ circles_of out @ lines_of out)

let test_ellipse_arc_no_output () =
  let out = output {|(ellipse_arc
    (center 100 100) (major_radius 50) (minor_radius 30) (rotation_angle 0)
    (start_angle 0) (end_angle 90)
    (stroke (width 0) (type default))
    (uuid "ffffffff-ffff-ffff-ffff-fffffffffffd")
  )|} in
  assert_equal ~msg:"ellipse_arc produces no drawn output" []
    (arcs_of out @ circles_of out @ lines_of out)

let test_embedded_fonts_no_output () =
  let out = output "(embedded_fonts no)" in
  assert_equal ~msg:"embedded_fonts produces no drawn output" []
    (arcs_of out @ circles_of out @ lines_of out)

let test_net_chain_no_output () =
  let out = output {|(net_chain "N$1" (from "R1" "1") (to "R2" "2"))|} in
  assert_equal ~msg:"net_chain produces no drawn output" []
    (arcs_of out @ circles_of out @ lines_of out)

let test_group_no_output () =
  let out = output {|(group "g1"
    (uuid "ffffffff-ffff-ffff-ffff-fffffffffffc")
    (members)
  )|} in
  assert_equal ~msg:"group produces no drawn output" []
    (arcs_of out @ circles_of out @ lines_of out)

(* ── Suite ────────────────────────────────────────────────────────────── *)

let suite = "KiCad V8 schematic parser" >:::
  [ "minimal v8 schematic parses"                  >:: test_minimal_parses
  ; "empty title_block produces no text"           >:: test_no_title_block_parses
  ; "partial title_block renders present fields"   >:: test_partial_title_block_parses
  (* Wire *)
  ; "wire segment: correct coordinates"            >:: test_wire_value
  (* Arc *)
  ; "arc: correct center/start/end/radius"         >:: test_arc_value
  ; "arc: collinear points produce no output"      >:: test_arc_collinear_no_output
  (* Circle *)
  ; "circle: correct center and radius"            >:: test_circle_value
  (* Bézier *)
  ; "bezier: exactly 16 line segments"             >:: test_bezier_16_segments
  ; "bezier: first segment starts at P0"           >:: test_bezier_starts_at_p0
  ; "bezier: last segment ends at P3"              >:: test_bezier_ends_at_p3
  (* Rule area *)
  ; "rule_area: all four square edges present"     >:: test_rule_area_value
  (* Text box *)
  ; "text_box (at+size): text and rect values"     >:: test_text_box_at_size_value
  ; "text_box (start+end): text and rect values"   >:: test_text_box_start_end_value
  (* Labels *)
  ; "label: text value and coordinates"            >:: test_label_value
  (* Skipped metadata *)
  ; "ellipse: no drawn output"                     >:: test_ellipse_no_output
  ; "ellipse_arc: no drawn output"                 >:: test_ellipse_arc_no_output
  ; "embedded_fonts: no drawn output"              >:: test_embedded_fonts_no_output
  ; "net_chain: no drawn output"                   >:: test_net_chain_no_output
  ; "group: no drawn output"                       >:: test_group_no_output
  ]

let _ = run_test_tt_main suite
