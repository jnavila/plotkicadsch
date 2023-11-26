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

let check_at = (fun (Coord(x1, y1), a1) (Coord(x2, y2), a2) -> assert_equal x1 x2; assert_equal y1 y2; assert_equal a1 a2)

let test_at = create_test pin_at_coord_expr check_at

let at_tests = test_list test_at
    [ ("(at 254 0 3)", (Coord (25400, 0), 3))
    ; ("(at 0 127)", (Coord (0, 12700), 0))
    ]
;;

let check_paper = (fun (Coord(x1, y1)) (Coord(x2, y2)) -> assert_equal ~printer:string_of_int x1 x2; assert_equal ~printer:string_of_int y1 y2)

let test_paper = create_test paper_expr check_paper

let paper_tests = test_list test_paper
    [ ({|  (paper "A4") |}, Coord (29700, 21000))
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
      ("(justify left bottom)", {horiz=Some J_left; vert=Some J_bottom})
    ; ("(justify right center)", {horiz=Some J_right; vert=Some J_center})
    ; ("(justify center top)", {horiz=Some J_center; vert=Some J_top})
    ; ("(justify  center top)", {horiz=Some J_center; vert=Some J_top})
    ; ("(justify center 	 top)", {horiz=Some J_center; vert=Some J_top})
    ; ("(justify left bottom )", {horiz=Some J_left; vert=Some J_bottom})
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
      ("(font (size 1.7526 1.7526))", {font=None; size=Coord(175, 175); italic=false; bold=false; kolor=None})
    ; ("(font (size 1.7526 1.7526) italic)", {font=None; size=Coord(175, 175); italic=true; bold=false; kolor=None})
    ; ("(font (size 1.7526 1.7526) bold)", {font=None; size=Coord(175, 175); italic=false; bold=true; kolor=None})
    ; ("(font (size 1.7526 1.7526) italic bold)", {font=None; size=Coord(175, 175); italic=true; bold=true; kolor=None})
    ; ("(font (size 1.7526 1.7526) italic bold (color 0 0 0 0))", {font=None; size=Coord(175, 175); italic=true; bold=true; kolor=Some{red=0; green=0; blue=0; alpha=0}})
    ; ("(font (size 1.7526 1.7526)    (color 0 0 0 0))", {font=None; size=Coord(175, 175); italic=false; bold=false; kolor=Some{red=0; green=0; blue=0; alpha=0}})
    ]

;;

let check_property (p1: property) (p2: property)  =
  assert_equal p1.name p2.name;
  assert_equal p1.value p2.value;
  assert_equal p1.id p2.id

let test_property = create_test property_expr check_property

let property_tests = List.map ~f:(fun (expr, p) -> (expr >:: (fun _ -> test_property expr p)))
    [
      ("(property \"Reference\" \"U\" (id 0) (at -4.6228 10.0584 0)
        (effects (font (size 1.7526 1.7526)) (justify left bottom)))", {name="Reference"; value="U"; id=0; at=(Coord(-182, 396)); rot =0; effects=Some {font={font=None; size=Coord(69, 69); italic=false; bold=false; kolor=None}; justify=Some {horiz=Some J_left; vert=Some J_bottom}; hide=false}})
    ; ("(property \"Reference\" \"U\" (id 0) (at -4.6228 10.0584 0))", {name="Reference"; value="U"; id=0; at=(Coord(-182, 396)); rot =0; effects=Some {font={font=None; size=Coord(69, 69); italic=false; bold=false; kolor=None}; justify=Some {horiz=Some J_left; vert=Some J_bottom}; hide=false}})
      ; ("(property \"Footprint\" \"\" (id 2) (at 0 0 0)
        (effects (font (size 1.27 1.27)) hide)
      )",
         {name="Footprint"; value=""; id=2; at=(Coord(0, 0)); rot=0; effects=Some {font={font=None; size=Coord(50, 50); italic=false; bold=false; kolor=None}; justify=None; hide=true}})
        ; ({|    (property "Références Inter-Feuilles" "${INTERSHEET_REFS}" (id 0) (at 71.6583 87.5506 0)
      (effects (font (size 1.27 1.27)) (justify left) hide)
    )|},
           {name="Références Inter-Feuilles"; value="${INTERSHEET_REFS}"; id=0; at=(Coord(716583, 875506)); rot=0; effects=Some {font={font=None; size=Coord(50, 50); italic=false; bold=false; kolor=None}; justify=None; hide=true}})
        ; ({|       (property "ki_description" "Power symbol creates a global label with name \"GNDPWR\" , global ground" (id 5) (at 0 0 0)) |},
           {name="ki_description"; value={|Power symbol creates a global label with name "GNDPWR" , global ground|}; id=5; at=(Coord(0, 0)); rot=0; effects=None})
        ; ({|      (property "Reference" "#PWR" (at 0 -6.35 0) (effects (font (size 1.27 1.27)) hide)) |},
           {name="Reference"; value="#PWR"; id=0; at=(Coord(0, -635)); rot=0; effects=Some {font={font=None; size=Coord(50, 50); italic=false; bold=false; kolor=None}; justify=None; hide=true}})
        ; ({|    (property "Références Inter-Feuilles" "${INTERSHEET_REFS}" (at 124.5394 76.3269 90)
      (effects (font (size 1.27 1.27)) (justify left) hide)
    ) |},
           {name="Références Inter-Feuilles"; value="${INTERSHEET_REFS}"; id=0; at=(Coord(1245394, 763269)); rot=90; effects=Some {font={font=None; size=Coord(50, 50); italic=false; bold=false; kolor=None}; justify=Some {horiz=Some J_left; vert=Some J_bottom}; hide=true}})

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
        )", Polygon (0, [ RelCoord(17876, 0); RelCoord(17876, 2032); RelCoord(17937, 2032); RelCoord(17937, 0); RelCoord(17876, 0)]))
    ]

;;

let check_pins p1 p2 =   match (p1, p2) with
  | ((Pin {name; number; length; contact=RelCoord (x1, y1); orient}), (Pin {name=name1; number=number1; length=length1; contact=RelCoord(x2, y2); orient=orient1})) ->(
      assert_equal name name1 ~printer:(fun (s, _) -> s);
      assert_equal number number1 ~printer:(fun (s, _) -> s);
      assert_equal length length1 ~printer:(fun (Size s) -> string_of_int s);
      assert_equal x1 x2 ~printer:string_of_int;
      assert_equal y1 y2 ~printer:string_of_int;
      assert_equal orient orient1 ~printer:show_pin_orientation)
  | _, _ -> assert_failure "pins should be Pins"

let test_pin = create_test pin_expr check_pins

let pin_tests = test_list test_pin
    [
      ({|(pin output line (at 17.78 2.54 180) (length 5.08)
          (name "S1" (effects (font (size 1.27 1.27))))
          (number "1" (effects (font (size 1.27 1.27))))
        )|}, Pin {name=("S1", Size 127); number=("1", Size 127); length=Size 508; contact=RelCoord(1778, 254); orient=P_L});
      ({|(pin input line (at -17.78 -5.08 0) (length 5.08)
          (name "G1" (effects (font (size 1.27 1.27))))
          (number "2" (effects (font (size 1.27 1.27))))
        )
|}, Pin {name=("G1", Size 127); number=("2", Size 127); length=Size 508; contact=RelCoord(-1778, -508); orient=P_R});
      ({|(pin bidirectional line (at 20.32 10.16 90) (length 5.08)
          (name "VCC" (effects (font (size 1.27 1.27))))
          (number "8" (effects (font (size 1.27 1.27))))
        )
|}, Pin {name=("VCC", Size 127); number=("8", Size 127); length=Size 508; contact=RelCoord(2032, 1016); orient=P_U});
      ({|(pin passive line (at 5.08 2.54 180) (length 3.81)
          (name "Pin_1" (effects (font (size 1.27 1.27))))
          (number "1" (effects (font (size 1.27 1.27))))
        )
|}, Pin {name=("Pin_1", Size 127); number=("1", Size 127); length=Size 381; contact=RelCoord(508, 254); orient=P_L});
       ({|(pin power_in line (at 5.08 0 180) (length 3.81)
          (name "Pin_2" (effects (font (size 1.27 1.27))))
          (number "2" (effects (font (size 1.27 1.27))))
        )
|}, Pin {name=("Pin_2", Size 127); number=("2", Size 127); length=Size 381; contact=RelCoord(508, 0); orient=P_L});
     ({|(pin passive line (at 5.08 -2.54 180) (length 3.81)
          (name "Pin_3" (effects (font (size 1.27 1.27))))
          (number "3" (effects (font (size 1.27 1.27))))
        ) |}
        , Pin {name=("Pin_3", Size 127); number=("3", Size 127); length=Size 381; contact=RelCoord(508, -254); orient=P_L})
      ; ({|(pin power_in line (at 0 0 270) (length 0) hide
          (name "GND" (effects (font (size 1.27 1.27))))
          (number "1" (effects (font (size 1.27 1.27))))) |}
        , Pin {name=("GND", Size 127); number=("1", Size 127); length=Size 0; contact=RelCoord(0, 0); orient=P_D})

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
      (RelCoord(-698, -381), 190, (-180.0, 0.0)))
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
       Arc {s=Size 50; radius=190; sp=RelCoord(-889, -381); ep=RelCoord(-508, -381); center=RelCoord(-698, -381)})
     ; ({|(arc (start -1.016 1.016) (end -1.016 -1.016) (radius (at -1.016 0) (length 1.016) (angles 90.1 -90.1))
          (stroke (width 0)) (fill (type outline))
        )|},
        Arc {s=Size 0; radius=101; sp=RelCoord(-101, 101); ep=RelCoord(-101, -101); center=RelCoord(-101, 0)})
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
            (xy 0.86 2.54)
          )
          (stroke (width 0.1524)) (fill (type none))
        )
|}, Bezier(15, [RelCoord(127, 254); RelCoord(86, 254)]))
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
       Circle (127, {center=RelCoord(0, 127); radius=127}))
    ; ({|(circle (center 0 1.27) (radius 1.27))
        |},
       Circle (10, {center=RelCoord(0, 127); radius=127}))
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
       Text {c=RelCoord(127, 635); text="mnt"; s=Size 127})
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
    )|}
      , { names=["Connector:Conn_01x03_Male"]
        ; draw_pnum=true
        ; draw_pname=false
        ; multi=false
        ; graph=List.init ~len:9 ~f:(fun _ -> {parts=1; prim=Field})})
    ; ({|
    (symbol "power:GND" (power) (pin_names (offset 0)) (in_bom yes) (on_board yes)
      (property "Reference" "#PWR" (id 0) (at 0 -6.35 0)
        (effects (font (size 1.27 1.27)) hide)
      )
      (property "Value" "GND" (id 1) (at 0 -3.81 0)
        (effects (font (size 1.27 1.27)))
      )
      (property "Footprint" "" (id 2) (at 0 0 0)
        (effects (font (size 1.27 1.27)) hide)
      )
      (property "Datasheet" "" (id 3) (at 0 0 0)
        (effects (font (size 1.27 1.27)) hide)
      )
      (property "ki_keywords" "global power" (id 4) (at 0 0 0)
        (effects (font (size 1.27 1.27)) hide)
      )
      (property "ki_description" "Power symbol creates a global label with name GND , ground" (id 5) (at 0 0 0)
        (effects (font (size 1.27 1.27)) hide)
      )
      (symbol "GND_0_1"
        (polyline
          (pts
            (xy 0 0)
            (xy 0 -1.27)
            (xy 1.27 -1.27)
            (xy 0 -2.54)
            (xy -1.27 -1.27)
            (xy 0 -1.27)
          )
          (stroke (width 0) (type default) (color 0 0 0 0))
          (fill (type none))
        )
      )
      (symbol "GND_1_1"
        (pin power_in line (at 0 0 270) (length 0) hide
          (name "GND" (effects (font (size 1.27 1.27))))
          (number "1" (effects (font (size 1.27 1.27))))
        )
      )
    )
 |}
      , { names=["power:GND"]
        ; draw_pnum=true
        ; draw_pname=false
        ; multi=false
        ; graph=List.init ~len:2 ~f:(fun _ -> {parts=1; prim=Field})})
    ]

;;

let test_junction = create_test junction_expr check_paper

let junction_tests = test_list test_junction
    [ ("(junction (at 254 0) (diameter 0.9144) (color 0 0 0 0))", Coord (25400, 0))
    ; ("(junction (at 49.53 151.13) (diameter 0) (color 0 0 0 0))", Coord (4953, 15113))
    ]

;;
let test_no_connect = create_test no_connect_expr check_paper

let no_connect_tests = test_list test_no_connect
    [ ("(no_connect (at 254 0) (uuid e8352a79-c40d-4cf1-97a7-2bb649ced79a))", Coord (25400, 0))
    ; ("(no_connect (at 49.53 151.13) (uuid e8352a79-c40d-4cf1-97a7-2bb649ced79a))", Coord (4953, 15113))
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
let test_lib_symbols = create_test lib_symbols_expr (fun _ _ -> ())

let lib_symbols_tests = test_list test_lib_symbols
 [ ({|   (lib_symbols
    (symbol "power:GND" (power) (pin_names (offset 0)) (in_bom yes) (on_board yes)
      (property "Reference" "#PWR" (at 0 -6.35 0)
        (effects (font (size 1.27 1.27)) hide)
      )
      (property "Value" "GND" (at 0 -3.81 0)
        (effects (font (size 1.27 1.27)))
      )
      (property "Footprint" "" (at 0 0 0)
        (effects (font (size 1.27 1.27)) hide)
      )
      (property "Datasheet" "" (at 0 0 0)
        (effects (font (size 1.27 1.27)) hide)
      )
      (property "ki_keywords" "global power" (at 0 0 0)
        (effects (font (size 1.27 1.27)) hide)
      )
      (property "ki_description" "Power symbol creates a global label with name GND , ground" (at 0 0 0)
        (effects (font (size 1.27 1.27)) hide)
      )
      (symbol "GND_0_1"
        (polyline
          (pts
            (xy 0 0)
            (xy 0 -1.27)
            (xy 1.27 -1.27)
            (xy 0 -2.54)
            (xy -1.27 -1.27)
            (xy 0 -1.27)
          )
          (stroke (width 0) (type default))
          (fill (type none))
        )
      )
      (symbol "GND_1_1"
        (pin power_in line (at 0 0 270) (length 0) hide
          (name "GND" (effects (font (size 1.27 1.27))))
          (number "1" (effects (font (size 1.27 1.27))))
        )
      )
    )
  ) |}, ())
 ]


;;
let test_pts = create_test pts_expr (fun l1 l2 -> List.iter2 ~f:(fun (Coord(x1, y1)) (Coord(x2, y2)) -> assert_equal x1 x2; assert_equal y1 y2) l1 l2)

let pts_tests = test_list test_pts
    [ ("(pts (xy 29.21 68.58) (xy 59.69 68.58))", [Coord(2921, 6858); Coord(5969, 6858)])
    ]

;;

let test_wire = create_test wire_expr (fun _ _ -> ())

let wire_tests = test_list test_wire

  [ ({|(wire (pts (xy 29.21 68.58) (xy 59.69 68.58))
    (stroke (width 0) (type default) (color 0 0 0 0))
    (uuid 78aef266-32d4-439c-9762-afe709cb660f)
  )|}, ())
  ; ({|(wire (pts (xy 29.21 68.58) (xy 59.69 68.58))
    (stroke (width 0) (type default))
    (uuid 78aef266-32d4-439c-9762-afe709cb660f)
  )|}, ())
  ]

;;

let check_global_label (Coord(x1, y1), r1, text1, (Size s1), shape1, j1) (Coord (x2, y2), r2, text2, (Size s2), shape2, j2) = assert_equal x1 x2 ~printer:string_of_int; assert_equal y1 y2 ~printer:string_of_int; assert_equal r1 r2 ~printer:string_of_int; assert_equal text1 text2 ~printer:(fun c->c); assert_equal s1 s2 ~printer:string_of_int

let test_global_test = create_test global_label_expr check_global_label

let global_label_tests = test_list test_global_test
    [ ({|  (global_label "test 1" (shape input) (at 63.5 87.63 0) (fields_autoplaced)
    (effects (font (size 1.27 1.27)) (justify left))
    (uuid 2cee9329-354d-4eab-887d-c615701cca76)
    (property "Références Inter-Feuilles" "${INTERSHEET_REFS}" (id 0) (at 71.6583 87.5506 0)
      (effects (font (size 1.27 1.27)) (justify left) hide)
    )
  )|}, (Coord(6350, 8763), 0, "test 1", (Size 127), InputPort, J_left))
    ; {|   (global_label "GO3" (shape output) (at 124.46 82.55 90) (fields_autoplaced)
    (effects (font (size 1.27 1.27)) (justify left))
    (uuid 10c8ec0b-9471-485f-995e-5254544feb64)
    (property "Intersheetrefs" "${INTERSHEET_REFS}" (at 124.46 82.55 0)
      (effects (font (size 1.27 1.27)) hide)
    )
    (property "Références Inter-Feuilles" "${INTERSHEET_REFS}" (at 124.5394 76.3269 90)
      (effects (font (size 1.27 1.27)) (justify left) hide)
    )
  ) |}, (Coord(12446, 8255), 90, "GO3", (Size 127), OutputPort, J_left)

        ]


let suite = "OUnit for " >:::
            List.concat
              [ yesno_tests
              ; uuid_tests
              ; paper_tests
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
              ; lib_symbols_tests
              ; wire_tests
              ; global_label_tests
            ]

let _ =
  run_test_tt_main suite
