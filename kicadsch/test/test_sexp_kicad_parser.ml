open OUnit
open! StdLabels
open Kicadsch.Sigs
open Kicadsch.Defs
open Kicadsch.Sexp
open Kicadsch.Lib_sigs
module Decode = Sexp_decode.Make(Base.Sexp)
(* module Csexp_i = Csexp.Make(Sexp) *)

let create_test parser checker =
  fun parse_expr expected_value ->
  let t = Parsexp.Single.parse_string parse_expr in
  match t with
  | Ok res -> (match Decode.run parser res with
      | Some r -> checker expected_value r
      | None -> assert_failure "expression not correctly structured"
    )
  | Error e -> assert_failure ( "not valid sexp " ^ (Parsexp__Parse_error.message e))

let test_list single_test test_list =
  List.map ~f: (fun (expr, j ) -> (expr >:: (fun _ -> (single_test expr j)))) test_list

;;
let check_yesno e1 e2 = assert_bool "yesno no match" ((e1 && e2) || (not e1 && not e2))

let test_yesno = create_test (yesno_expr "foo") check_yesno

let yesno_tests = test_list test_yesno
    [ ({|(foo yes) |}, true)
    ; ({|(foo no)|}, false)
    ]
;;
let make_uuid u =   match Uuidm.of_string u with
  | Some uuid2 -> uuid2
  | None -> raise (Invalid_argument ("internal error " ^ u))


let check_uuid u1 u2 = assert_bool "uuid do not match" (Uuidm.compare u1 u2 = 0)
let test_uuid = create_test uuid_expr check_uuid

let uuid_tests = test_list test_uuid
    [ ({|(uuid 072ad1ed-9172-426c-8e5d-41f1dcd4b625)|}, make_uuid "072ad1ed-9172-426c-8e5d-41f1dcd4b625")
    ]
let check_kolor =  (fun k1 k2 ->
       assert_equal k1.alpha k2.alpha
     ; assert_equal k1.red k2.red
     ; assert_equal k1.green k2.green
     ; assert_equal k1.blue k2.blue)

let test_kolor = create_test kolor_expr check_kolor

let color_tests = test_list test_kolor
    [
      ("(color 1 2 3 4)", {red=1; green=2; blue=3; alpha=4})
    ; ("(color 4 3 2 1)", {red=4; green=3; blue=2; alpha=1})
    ]
;;

let check_at = (fun (Coord(x1, y1)) (Coord(x2, y2)) -> assert_equal x1 x2; assert_equal y1 y2)

let test_at = create_test pin_at_coord_expr check_at

let at_tests = test_list test_at
    [ ("(at 254 0 3)", Coord (10_000, 0))
    ; ("(at 0 127)", Coord (0, 5000))
    ]
;;

let check_fill f1 f2 =
  assert_equal f1.fill_type f2.fill_type;
  assert_equal f1.kolor f2.kolor

let test_fill = create_test fill_expr check_fill

let fill_tests = test_list test_fill
    [ ("(fill (type outline))", {fill_type=Outline_fill; kolor=None})
    ; ("(fill (type none))", {fill_type=No_fill; kolor=None})
    ; ("(fill (type background))", {fill_type=Background_fill; kolor=None})
    ]

;;

let check_justif = fun j {horiz; vert} -> assert_equal horiz j.horiz; assert_equal vert j.vert

let test_justif = create_test justify_expr check_justif

let justif_tests = test_list test_justif
    [
      ("(justify left bottom)", {horiz=J_left; vert=J_bottom})
    ; ("(justify right center)", {horiz=J_right; vert=J_center})
    ; ("(justify center top)", {horiz=J_center; vert=J_top})
    ; ("(justify  center top)", {horiz=J_center; vert=J_top})
    ; ("(justify center 	 top)", {horiz=J_center; vert=J_top})
    ; ("(justify left bottom )", {horiz=J_left; vert=J_bottom})
    ]
;;

let check_font f {font; size; italic; bold; kolor} =
    let Coord(x, y) = size in
    let Coord(x_r, y_r) = f.size in
    assert_equal italic f.italic;
    assert_equal bold f.bold;
    assert_equal x x_r;
    assert_equal y y_r;
    (match font, f.font with
    | None, None -> ()
    | None, Some _ -> assert_failure "font name not found"
    | Some _, None -> assert_failure "spurious font name"
    | Some n1, Some n2 -> assert_equal n1 n2)

let test_font = create_test font_expr check_font


let font_tests =
  test_list test_font
    [
      ("(font (size 1.7526 1.7526))", {font=None; size=Coord(69, 69); italic=false; bold=false; kolor=None})
    ; ("(font (size 1.7526 1.7526) italic)", {font=None; size=Coord(69, 69); italic=true; bold=false; kolor=None})
    ; ("(font (size 1.7526 1.7526) bold)", {font=None; size=Coord(69, 69); italic=false; bold=true; kolor=None})
    ; ("(font (size 1.7526 1.7526) italic bold)", {font=None; size=Coord(69, 69); italic=true; bold=true; kolor=None})
    ; ("(font (size 1.7526 1.7526) italic bold (color 0 0 0 0))", {font=None; size=Coord(69, 69); italic=true; bold=true; kolor=Some{red=0; green=0; blue=0; alpha=0}})
    ; ("(font (size 1.7526 1.7526)    (color 0 0 0 0))", {font=None; size=Coord(69, 69); italic=false; bold=false; kolor=Some{red=0; green=0; blue=0; alpha=0}})
    ]
;;
;;

let check_property (p1: property) (p2: property)  =
  assert_equal p1.name p2.name;
  assert_equal p1.value p2.value;
  assert_equal p1.id p2.id

let test_property = create_test property_expr check_property

let property_tests = List.map ~f:(fun (expr, p) -> (expr >:: (fun _ -> test_property expr p)))
    [
      ("(property \"Reference\" \"U\" (id 0) (at -4.6228 10.0584 0)
        (effects (font (size 1.7526 1.7526)) (justify left bottom)))", {name="Reference"; value="U"; id=0; at=(Coord(-182, 396)); effects=Some {font={font=None; size=Coord(69, 69); italic=false; bold=false; kolor=None}; justify=Some {horiz=J_left; vert=J_bottom}; hide=false}})
    ; ("(property \"Reference\" \"U\" (id 0) (at -4.6228 10.0584 0))", {name="Reference"; value="U"; id=0; at=(Coord(-182, 396)); effects=Some {font={font=None; size=Coord(69, 69); italic=false; bold=false; kolor=None}; justify=Some {horiz=J_left; vert=J_bottom}; hide=false}})
      ; ("(property \"Footprint\" \"\" (id 2) (at 0 0 0)
        (effects (font (size 1.27 1.27)) hide)
      )",
         {name="Footprint"; value=""; id=2; at=(Coord(0, 0)); effects=Some {font={font=None; size=Coord(50, 50); italic=false; bold=false; kolor=None}; justify=None; hide=true}})

        ]

;;

let assert_array_equal points1 points2 =
  List.iter ~f:(fun ((RelCoord (x1, y1)), (RelCoord(x2, y2))) -> assert_equal x1 x2; assert_equal y1 y2) (List.combine points1 points2)

let assert_list_equal list1 list2 =
  List.iter ~f:(fun (elt1, elt2) -> assert_equal elt1 elt2) (List.combine list1 list2)


let check_rectangle shape1 shape2  =
  match shape1, shape2 with
  | (Polygon (width1, points1)), (Polygon (width2, points2)) ->
      assert_equal width1 width2;
      assert_array_equal points1 points2
  | _, _ -> assert_failure "rectangle should be a polygon"

let test_rectangle = create_test rectangle_expr check_rectangle

let rectangle_tests = test_list test_rectangle
    [
      ("(rectangle (start 178.7652 0) (end 179.3748 20.32)
          (stroke (width 0)) (fill (type outline))
        )", Polygon (0, [ RelCoord(7038, 0); RelCoord(7038, 800); RelCoord(7062, 800); RelCoord(7062, 0); RelCoord(7038, 0)]))
    ]

;;

let check_pins p1 p2 =   match (p1, p2) with
  | ((Pin {name; number; length; contact=RelCoord (x1, y1); orient}), (Pin {name=name1; number=number1; length=length1; contact=RelCoord(x2, y2); orient=orient1})) ->(
      assert_equal name name1;
      assert_equal number number1;
      assert_equal length length1;
      assert_equal x1 x2;
      assert_equal y1 y2;
      assert_equal orient orient1)
  | _, _ -> assert_failure "pins should be Pins"

let test_pin = create_test pin_expr check_pins

let pin_tests = test_list test_pin
    [
      ({|(pin output line (at 17.78 2.54 180) (length 5.08)
          (name "S1" (effects (font (size 1.27 1.27))))
          (number "1" (effects (font (size 1.27 1.27))))
        )|}, Pin {name=("S1", Size 50); number=("1", Size 50); length=Size 200; contact=RelCoord(700, 100); orient=P_L});
      ({|(pin input line (at -17.78 -5.08 0) (length 5.08)
          (name "G1" (effects (font (size 1.27 1.27))))
          (number "2" (effects (font (size 1.27 1.27))))
        )
|}, Pin {name=("G1", Size 50); number=("2", Size 50); length=Size 200; contact=RelCoord(-700, -200); orient=P_R});
      ({|(pin bidirectional line (at 20.32 10.16 90) (length 5.08)
          (name "VCC" (effects (font (size 1.27 1.27))))
          (number "8" (effects (font (size 1.27 1.27))))
        )
|}, Pin {name=("VCC", Size 50); number=("8", Size 50); length=Size 200; contact=RelCoord(800, 400); orient=P_U});
      ({|(pin passive line (at 5.08 2.54 180) (length 3.81)
          (name "Pin_1" (effects (font (size 1.27 1.27))))
          (number "1" (effects (font (size 1.27 1.27))))
        )
|}, Pin {name=("Pin_1", Size 50); number=("1", Size 50); length=Size 150; contact=RelCoord(200, 100); orient=P_L});
       ({|(pin passive line (at 5.08 0 180) (length 3.81)
          (name "Pin_2" (effects (font (size 1.27 1.27))))
          (number "2" (effects (font (size 1.27 1.27))))
        )
|}, Pin {name=("Pin_2", Size 50); number=("2", Size 50); length=Size 150; contact=RelCoord(200, 0); orient=P_L});
     ({|(pin passive line (at 5.08 -2.54 180) (length 3.81)
          (name "Pin_3" (effects (font (size 1.27 1.27))))
          (number "3" (effects (font (size 1.27 1.27))))
        ) |}, Pin {name=("Pin_3", Size 50); number=("3", Size 50); length=Size 150; contact=RelCoord(200, -100); orient=P_L})
    ]

;;

let check_angles (as1, ae1) (as2, ae2) =
  assert_equal as1 as2;
  assert_equal ae1 ae2

let test_angles = create_test angles_expr check_angles

let angles_tests = test_list test_angles
    [ ({|(angles -180.0 0.0) |}, (-180.0, 0.0))
    ]
;;

let check_radius (RelCoord(x1, y1), length1, (as1, ae1)) (RelCoord(x2, y2), length2, (as2, ae2)) =
  assert_equal ~msg:"x" x1 x2;
  assert_equal ~msg:"y" y1 y2;
  assert_equal ~msg:"angle start" as1 as2;
  assert_equal ~msg:"angle end" ae1 ae2;
  assert_equal ~msg:"length" length1 length2

let test_radius = create_test radius_expr check_radius

let radius_tests = test_list test_radius
    [({|(radius (at -6.985 -3.81) (length 1.905) (angles -180.0 0.0)) |},
      (RelCoord(-275, -150), 75, (-180.0, 0.0)))
    ]
;;


let check_arcs a1 a2 =   match (a1, a2) with
  |Arc {s=Size s1; radius=radius1; sp=RelCoord(xs1, ys1); ep=RelCoord(xe1,ye1); center=RelCoord(xc1, yc1)}, Arc {s=Size s2; radius=radius2; sp=RelCoord(xs2, ys2); ep=RelCoord(xe2,ye2); center=RelCoord(xc2, yc2)} ->
    assert_equal ~msg:"s1" s1 s2;
    assert_equal ~msg:"radius1" radius1 radius2;
    assert_equal ~msg:"xs1" xs1 xs1;
    assert_equal ~msg:"xe1" xe1 xe2;
    assert_equal ~msg:"xc1" xc1 xc2
  |_, _ -> assert_failure "not arcs!"

let test_arc = create_test arc_expr check_arcs

let arc_tests = test_list test_arc
    [ ({|(arc (start -8.89 -3.81) (end -5.08 -3.81) (radius (at -6.985 -3.81) (length 1.905) (angles -179.9 -0.1))
          (stroke (width 0.508)) (fill (type none))
        ) |},
       Arc {s=Size 20; radius=75; sp=RelCoord(-350, -150); ep=RelCoord(-200, -150); center=RelCoord(-275, -150)})
     ; ({|(arc (start -1.016 1.016) (end -1.016 -1.016) (radius (at -1.016 0) (length 1.016) (angles 90.1 -90.1))
          (stroke (width 0)) (fill (type outline))
        )|},
        Arc {s=Size 0; radius=40; sp=RelCoord(-40, 40); ep=RelCoord(-40, -40); center=RelCoord(-40, 0)})
    ]
;;

let check_bezier c1 c2 = match (c1, c2) with
  | Bezier (w1, points1), Bezier (w2, points2) ->
    assert_equal w1 w2;
    assert_array_equal points1 points2
  | _, _  -> failwith "must be a Bezier!"

let test_bezier = create_test bezier_expr check_bezier

let bezier_tests = test_list test_bezier
    [ ({|(gr_curve
          (pts
            (xy 1.27 2.54)
            (xy 0.8636 2.54)
          )
          (stroke (width 0.1524)) (fill (type none))
        )
|}, Bezier(6, [RelCoord(50, 100); RelCoord(34, 100)]))
    ]
;;

let check_circles c1 c2 =   match (c1, c2) with
  | Circle (w1, {center=RelCoord(x1, y1); radius=radius1}), Circle (w2, {center=RelCoord(x2, y2); radius = radius2}) ->
    assert_equal w1 w2;
    assert_equal x1 x2;
    assert_equal y1 y2;
    assert_equal radius1 radius2
  | _, _  -> failwith "must be a circle!"

let test_circle = create_test circle_expr check_circles

let circle_tests = test_list test_circle
    [ ({|(circle (center 0 1.27) (radius 1.27) (stroke (width 1.27)) (fill (type none)))
        |},
       Circle (50, {center=RelCoord(0, 50); radius=50}))
    ; ({|(circle (center 0 1.27) (radius 1.27))
        |},
       Circle (10, {center=RelCoord(0, 50); radius=50}))
    ]
;;

let check_text c1 c2 = match (c1, c2) with
  | Text {c=RelCoord(x1, y1); text=t1; s=Size s1}, Text {c=RelCoord(x2, y2); text=t2; s=Size s2} ->
    assert_equal ~msg:"x1" x1 x2;
    assert_equal ~msg:"y1" y1 y2;
    assert_equal ~msg:"t1" t1 t2;
    assert_equal ~msg:"s1" s1 s2
  | _, _  -> failwith "must be a Text!"

let test_text = create_test text_expr check_text

let text_tests = test_list test_text
    [ ({|(text "mnt" (at 1.27 6.35 0)
          (effects (font (size 1.27 1.27)))
        ) |},
       Text {c=RelCoord(50, 250); text="mnt"; s=Size 50})
    ]

;;

let check_unit u1 u2 =
  assert_equal (List.length u1) (List.length u2)

let test_unit = create_test unit_expr check_unit

let unit_tests = test_list test_unit
    [
    ({|(symbol "Conn_01x03_Male_1_1"
        (rectangle (start 0.8636 -2.413) (end 0 -2.667)
          (stroke (width 0.1524)) (fill (type outline))
        )
        (rectangle (start 0.8636 0.127) (end 0 -0.127)
          (stroke (width 0.1524)) (fill (type outline))
        )
        (rectangle (start 0.8636 2.667) (end 0 2.413)
          (stroke (width 0.1524)) (fill (type outline))
        )
        (polyline
          (pts
            (xy 1.27 -2.54)
            (xy 0.8636 -2.54)
          )
          (stroke (width 0.1524)) (fill (type none))
        )
        (polyline
          (pts
            (xy 1.27 0)
            (xy 0.8636 0)
          )
          (stroke (width 0.1524)) (fill (type none))
        )
        (polyline
          (pts
            (xy 1.27 2.54)
            (xy 0.8636 2.54)
          )
          (stroke (width 0.1524)) (fill (type none))
        )
        (pin passive line (at 5.08 2.54 180) (length 3.81)
          (name "Pin_1" (effects (font (size 1.27 1.27))))
          (number "1" (effects (font (size 1.27 1.27))))
        )
        (pin passive line (at 5.08 0 180) (length 3.81)
          (name "Pin_2" (effects (font (size 1.27 1.27))))
          (number "2" (effects (font (size 1.27 1.27))))
        )
        (pin passive line (at 5.08 -2.54 180) (length 3.81)
          (name "Pin_3" (effects (font (size 1.27 1.27))))
          (number "3" (effects (font (size 1.27 1.27))))
        )
      )
|},
     List.init ~len:9 ~f:(fun _ -> Field))
    ; (    {|(symbol "Conn_01x03_Male_1_1"
        (rectangle (start 0.8636 -2.413) (end -0.8636 2.413)))|}, [ Field ])
    ; (    {|(symbol "Conn_01x03_Male_1_1") |}, [ ])

  ]

;;

let check_component c1 c2 =
  let { names=names1
      ; draw_pnum=draw_pnum1
      ; draw_pname= draw_pname1
      ; multi = multi1
      ; graph = graph1
      } = c1 and
      { names=names2
      ; draw_pnum=draw_pnum2
      ; draw_pname= draw_pname2
      ; multi = multi2
      ; graph = graph2
      } = c2 in
  assert_equal draw_pname1 draw_pname2;
  assert_equal draw_pnum1 draw_pnum2;
  assert_equal multi1 multi2;
  assert_list_equal names1 names2;
  assert_equal (List.length graph1) (List.length graph2)

;;

let test_component = create_test symbol_expr check_component

let component_tests = test_list test_component
    [ ({|(symbol "Connector:Conn_01x03_Male" (pin_names (offset 1.016) hide) (in_bom yes) (on_board yes)
      (property "Reference" "J" (id 0) (at 0 5.08 0)
        (effects (font (size 1.27 1.27)))
      )
      (property "Value" "Conn_01x03_Male" (id 1) (at 0 -5.08 0)
        (effects (font (size 1.27 1.27)))
      )
      (property "Footprint" "" (id 2) (at 0 0 0)
        (effects (font (size 1.27 1.27)) hide)
      )
      (property "Datasheet" "~" (id 3) (at 0 0 0)
        (effects (font (size 1.27 1.27)) hide)
      )
      (property "ki_keywords" "connector" (id 4) (at 0 0 0)
        (effects (font (size 1.27 1.27)) hide)
      )
      (property "ki_description" "Generic connector, single row, 01x03, script generated (kicad-library-utils/schlib/autogen/connector/)" (id 5) (at 0 0 0)
        (effects (font (size 1.27 1.27)) hide)
      )
      (property "ki_fp_filters" "Connector*:*_1x??_*" (id 6) (at 0 0 0)
        (effects (font (size 1.27 1.27)) hide)
      )
      (symbol "Conn_01x03_Male_1_1"
        (rectangle (start 0.8636 -2.413) (end 0 -2.667)
          (stroke (width 0.1524)) (fill (type outline))
        )
        (rectangle (start 0.8636 0.127) (end 0 -0.127)
          (stroke (width 0.1524)) (fill (type outline))
        )
        (rectangle (start 0.8636 2.667) (end 0 2.413)
          (stroke (width 0.1524)) (fill (type outline))
        )
        (polyline
          (pts
            (xy 1.27 -2.54)
            (xy 0.8636 -2.54)
          )
          (stroke (width 0.1524)) (fill (type none))
        )
        (polyline
          (pts
            (xy 1.27 0)
            (xy 0.8636 0)
          )
          (stroke (width 0.1524)) (fill (type none))
        )
        (polyline
          (pts
            (xy 1.27 2.54)
            (xy 0.8636 2.54)
          )
          (stroke (width 0.1524)) (fill (type none))
        )
        (pin passive line (at 5.08 2.54 180) (length 3.81)
          (name "Pin_1" (effects (font (size 1.27 1.27))))
          (number "1" (effects (font (size 1.27 1.27))))
        )
        (pin passive line (at 5.08 0 180) (length 3.81)
          (name "Pin_2" (effects (font (size 1.27 1.27))))
          (number "2" (effects (font (size 1.27 1.27))))
        )
        (pin passive line (at 5.08 -2.54 180) (length 3.81)
          (name "Pin_3" (effects (font (size 1.27 1.27))))
          (number "3" (effects (font (size 1.27 1.27))))
        )
      )
    )
|}, { names=["Connector:Conn_01x03_Male"]
    ; draw_pnum=true
    ; draw_pname=false
    ; multi=false
    ; graph=List.init ~len:9 ~f:(fun _ -> {parts=1; prim=Field})})
        ]

;;

let test_junction = create_test junction_expr check_at

let junction_tests = test_list test_junction
    [ ("(junction (at 254 0) (diameter 0.9144) (color 0 0 0 0))", Coord (10_000, 0))
    ; ("(junction (at 49.53 151.13) (diameter 0) (color 0 0 0 0))", Coord (1950, 5950))
    ]

;;
let test_no_connect = create_test no_connect_expr check_at

let no_connect_tests = test_list test_no_connect
    [ ("(no_connect (at 254 0) (uuid e8352a79-c40d-4cf1-97a7-2bb649ced79a))", Coord (10_000, 0))
    ; ("(no_connect (at 49.53 151.13) (uuid e8352a79-c40d-4cf1-97a7-2bb649ced79a))", Coord (1950, 5950))
    ]

;;

let test_sch_pin = create_test sch_pin_expr (fun _ _ -> ())

let sch_pin_tests = test_list test_sch_pin
    [ ({|(pin "1" (uuid 7829cc5e-c0ba-483b-8ef1-279a374fd70e))|}, () )
    ]

;;

let test_sch_symbol = create_test sch_symbol_expr (fun _ _ -> ())

let sch_symbol_tests = test_list test_sch_symbol
    [ ({|(symbol (lib_id "Switch:SW_Rotary12") (at 123.19 101.6 180) (unit 1)
    (in_bom yes) (on_board yes) (fields_autoplaced)
    (uuid d1df6972-c898-47bb-9c11-b133529d84e6)
    (property "Reference" "SW?" (id 0) (at 126.0475 81.28 0))
    (property "Value" "SW_Rotary12" (id 1) (at 126.0475 83.82 0))
    (property "Footprint" "" (id 2) (at 128.27 119.38 0)
      (effects (font (size 1.27 1.27)) hide)
    )
    (property "Datasheet" "http://cdn-reichelt.de/documents/datenblatt/C200/DS-Serie%23LOR.pdf" (id 3) (at 128.27 119.38 0)
      (effects (font (size 1.27 1.27)) hide)
    )
    (pin "1" (uuid 7829cc5e-c0ba-483b-8ef1-279a374fd70e))
    (pin "10" (uuid ce111db8-722a-4485-a3c1-65a8695aff2c))
    (pin "11" (uuid 9d1189e7-dcd1-4d13-ac76-84afc907eac9))
    (pin "12" (uuid cb6c106f-8938-4a81-9e6f-86ef570eb0f8))
    (pin "13" (uuid 7449147b-2736-425b-8918-55edf88f3417))
    (pin "2" (uuid b74cd7b3-8d88-45b4-8f43-57735f14c07f))
    (pin "3" (uuid 39673e95-bfc6-4eec-bc62-e81ddb94e65a))
    (pin "4" (uuid 7dca9bf3-904d-4a2e-8829-04a976367986))
    (pin "5" (uuid e8e8eb28-ed91-4674-b2dc-fbbb637f75f7))
    (pin "6" (uuid 9688b964-3dc7-4fc1-9167-8c2ef1ae7bac))
    (pin "7" (uuid 9712e705-cada-4c71-8e1b-e8b5d0669b50))
    (pin "8" (uuid 39105380-e6cd-4a90-9748-cb3de7e4ffa2))
    (pin "9" (uuid b408cfea-98f0-460b-8b24-4722609632a0))
  )|}, ())
    ]
;;

let suite = "OUnit for " >:::
            List.concat
              [ yesno_tests
              ; uuid_tests
              ; at_tests
              ; fill_tests
              ; justif_tests
              ; color_tests
              ; font_tests
              ; property_tests
              ; rectangle_tests
              ; pin_tests
              ; angles_tests
              ; radius_tests
              ; arc_tests
              ; bezier_tests
              ; circle_tests
              ; text_tests
              ; unit_tests
              ; component_tests
              ; junction_tests
              ; no_connect_tests
              ; sch_pin_tests
              ; sch_symbol_tests
            ]

let _ =
  run_test_tt_main suite
