open OUnit
open! StdLabels
open Kicadsch.Sigs
open Kicadsch.NewKicad
open Kicadsch.Lib_sigs
open Angstrom

let create_test parser checker =
  fun parse_expr expected_value ->
    let t = parse_string ~consume:Consume.All parser parse_expr in
    match t with
    | Ok res  -> checker expected_value res
    | Error e -> assert_failure ("not parsed: " ^ e)

let test_list single_test test_list =
  List.map ~f: (fun (expr, j ) -> (expr >:: (fun _ -> (single_test expr j)))) test_list
;;

let check_string s1 s2 = assert_equal s1 s2

let test_string = create_test (string_expr "test") check_string

let string_tests = test_list test_string
    [ ("(test \"toto\")", "toto")              (* standard *)
    ; ("(test  \"toto\")", "toto")             (* white spaces in middle *)
    ; ("( test \"toto\")", "toto")             (* white space at start *)
    ; ("(test \"toto\" )", "toto")             (* white space at end *)
    ; ("(test \"toto\\\"\")", "toto\"")        (* escaped string *)
    ]
;;

let test_int ()=
  let t = parse_string ~consume:Consume.All (int_expr "test") "(test 123)" in
  match t with
  | Ok 123 -> ()
  | _ -> assert_failure "not parsed"

;;
let check_kolor =  (fun k1 k2 ->
       assert_equal k1.alpha k2.alpha
     ; assert_equal k1.red k2.red
     ; assert_equal k1.green k2.green
     ; assert_equal k1.blue k2.blue)

let test_kolor = create_test color_expr check_kolor

let color_tests = test_list test_kolor
    [
      ("(color 0 0 0 0)", {red=0; green=0; blue=0; alpha=0})
    ]
;;

let check_at = (fun (Coord(x1, y1)) (Coord(x2, y2)) -> assert_equal x1 x2; assert_equal y1 y2)

let test_at = create_test at_coord_expr check_at

let at_tests = test_list test_at
    [
      ("(at 0 0 0)", Coord (0, 0))
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

let check_property (p:property) {name; value; id; _} = assert_equal name p.name; assert_equal value p.value; assert_equal id p.id

let test_property = create_test property_expr check_property

let property_tests = List.map ~f:(fun (expr, p) -> (expr >:: (fun _ -> test_property expr p)))
    [
      ("(property \"Reference\" \"U\" (id 0) (at -4.6228 10.0584 0)
        (effects (font (size 1.7526 1.7526)) (justify left bottom)))", {name="Reference"; value="U"; id=0; at=(Coord(-182, 396)); effects=Some {font={font=None; size=Coord(69, 69); italic=false; bold=false; kolor=None}; justify=Some {horiz=J_left; vert=J_bottom}; hide=false}})
    ; ("(property \"Reference\" \"U\" (id 0) (at -4.6228 10.0584 0))", {name="Reference"; value="U"; id=0; at=(Coord(-182, 396)); effects=Some {font={font=None; size=Coord(69, 69); italic=false; bold=false; kolor=None}; justify=Some {horiz=J_left; vert=J_bottom}; hide=false}})
    ]

;;

let check_rectangle shape1 shape2  =
  match shape1, shape2 with
  | (Polygon (width1, points1)), (Polygon (width2, points2)) -> (
    assert_equal width1 width2;
    List.iter ~f:(fun ((RelCoord (x1, y1)), (RelCoord(x2, y2))) -> assert_equal x1 x2; assert_equal y1 y2) (List.combine points1 points2))
  | _, _ -> assert_failure "rectangle should be a polygon"

let test_rectangle = create_test rectangle_expr check_rectangle

let rectangle_tests = test_list test_rectangle
    [
      ("        (rectangle (start 178.7652 0) (end 179.3748 20.32)
          (stroke (width 0)) (fill (type outline))
        )", Polygon (0, [ RelCoord(7038, 0); RelCoord(7038, 800); RelCoord(7062, 800); RelCoord(7062, 0); RelCoord(7038, 0)]))
    ]

;;


let suite = "OUnit for " >:::
            List.concat [[ "int expression" >:: test_int]
                        ; string_tests
                        ; at_tests
                        ; justif_tests
                        ; color_tests
                        ; font_tests
                        ; property_tests
                        ; rectangle_tests
                        ]

let _ =
  run_test_tt_main suite
