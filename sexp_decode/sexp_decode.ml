(**
    Copyright © Inria 2022

   @author Benoît Montagu <benoit.montagu@inria.fr>
*)

include Sexp_decode_intf

module Make (X : SEXP) : S with type sexp := X.t = struct
  type sexp = X.t
  type state = sexp list list
  type 'a decode_res = Err of sexp | GenErr | SomeRes of 'a
  type 'a decoder = state -> (state * 'a) decode_res

  let run_with_result (decode: 'a decoder) (sexp: sexp) : ('a, sexp) result =
    match decode [ [ sexp ] ] with
    | GenErr -> Error sexp
    | Err s -> Error s
    | SomeRes ((a :: _) :: _, _) -> Error a
    | SomeRes ([], _) -> assert false
    | SomeRes ([] :: _, v) -> Ok v

  let run_list (decode : 'a decoder) (sexps : sexp list) : 'a option =
    match decode [ sexps ] with
    | GenErr| Err _ | SomeRes((_ :: _) :: _, _) -> None
    | SomeRes ([], _) -> assert false
    | SomeRes ([] :: _, v) -> Some v

  let run (decode : 'a decoder) (sexp : sexp) : 'a option =
    run_list decode [ sexp ]

  let return (v : 'a) : 'a decoder = fun rem -> SomeRes (rem, v)
  let const = return

  let%test _ = run_list (return 42) [] = Some 42
  let%test _ = run_list (return 42) [ Atom "foo" ] = None
  let%test _ = run (return 42) (Atom "foo") = None

  let error : 'a decoder = function
    | (state::_)::_ -> Err state
    | _ -> GenErr

  let%test _ = run_list error [] = None
  let%test _ = run_list error [ Atom "foo" ] = None
  let%test _ = run error (Atom "foo") = None

  let fail_default (default : 'a) (decode : 'b decoder) : 'a decoder =
   fun state ->
    match decode state with SomeRes ((state::_)::_,_) -> Err state | SomeRes _ | GenErr | Err _ -> SomeRes (state, default)

  let fail (decode : 'a decoder) : unit decoder = fail_default () decode

  let try_ (decode : 'a decoder) : 'a decoder =
   fun state ->
    match decode state with
    | SomeRes (_, v) -> SomeRes (state, v)
    | GenErr | Err _ as err -> err

  let map (f : 'a -> 'b) (decode : 'a decoder) : 'b decoder =
    fun state ->
    match decode state with
    | SomeRes (s, v) -> SomeRes (s, f v)
    | GenErr | Err _ as err  -> err

  let flip_map decode f = map f decode
  let ( >>| ) = flip_map
  let ( let+ ) = flip_map

  let%test _ = run_list (return 42 >>| ( + ) 1) [] = Some 43
  let%test _ = run_list (return 42 >>| ( + ) 1) [ Atom "foo" ] = None
  let%test _ = run (return 42 >>| ( + ) 1) (Atom "foo") = None

  let bind (decode : 'a decoder) (f : 'a -> 'b decoder) : 'b decoder =
   fun state ->
    match decode state with GenErr | Err _ as err  -> err | SomeRes (state, res) -> f res state

  let ( >>= ) = bind
  let ( let* ) = bind

  let%test _ = run_list (return 42 >>= fun n -> return (n + 1)) [] = Some 43

  let%test _ =
    run_list (return 42 >>= fun n -> return (n + 1)) [ Atom "foo" ] = None

  let%test _ = run (return 42 >>= fun n -> return (n + 1)) (Atom "foo") = None

  let seq (decode1 : 'a decoder) (decode2 : 'b decoder) : 'b decoder =
   fun state ->
    match decode1 state with GenErr | Err _ as err  -> err | SomeRes (state, _) -> decode2 state

  let ( >>> ) = seq

  let%test _ = run_list (return 42 >>> return 43) [] = Some 43

  let drop (decode : 'a decoder) : unit decoder = decode >>> return ()

  let ( <<< ) (decode1 : 'a decoder) (decode2 : 'b decoder) : 'a decoder =
    let* v1 = decode1 in
    let+ _v2 = decode2 in
    v1

  let%test _ = run_list (return 42 <<< return 43) [] = Some 42

  let or_else (decode1 : 'a decoder) (decode2 : 'a decoder) : 'a decoder =
   fun state ->
    match decode1 state with SomeRes _ as ok -> ok | GenErr | Err _ -> decode2 state

  let ( |+> ) = or_else

  let first (decodes : 'a decoder list) : 'a decoder =
    List.fold_left (fun acc d -> acc |+> d) error decodes

  let or_else_delayed (decode1 : 'a decoder) (decode2 : unit -> 'a decoder) :
      'a decoder =
   fun state ->
    match decode1 state with SomeRes _ as ok -> ok | GenErr | Err _ -> decode2 () state

  let ( |+>> ) = or_else_delayed

  let pair (decode1 : 'a decoder) (decode2 : 'b decoder) : ('a * 'b) decoder =
    let* v1 = decode1 in
    let+ v2 = decode2 in
    (v1, v2)

  let ( <*> ) = pair
  let tuple2 = pair

  let%test _ = run_list (return 42 <*> return 43) [] = Some (42, 43)

  let tuple3 (d1 : 'a1 decoder) (d2 : 'a2 decoder) (d3 : 'a3 decoder) :
      ('a1 * 'a2 * 'a3) decoder =
    let* v1 = d1 in
    let* v2 = d2 in
    let+ v3 = d3 in
    (v1, v2, v3)

  let tuple4 (d1 : 'a1 decoder) (d2 : 'a2 decoder) (d3 : 'a3 decoder)
      (d4 : 'a4 decoder) : ('a1 * 'a2 * 'a3 * 'a4) decoder =
    let* v1 = d1 in
    let* v2 = d2 in
    let* v3 = d3 in
    let+ v4 = d4 in
    (v1, v2, v3, v4)

  let tuple5 (d1 : 'a1 decoder) (d2 : 'a2 decoder) (d3 : 'a3 decoder)
      (d4 : 'a4 decoder) (d5 : 'a5 decoder) :
      ('a1 * 'a2 * 'a3 * 'a4 * 'a5) decoder =
    let* v1 = d1 in
    let* v2 = d2 in
    let* v3 = d3 in
    let* v4 = d4 in
    let+ v5 = d5 in
    (v1, v2, v3, v4, v5)

  let tuple6 (d1 : 'a1 decoder) (d2 : 'a2 decoder) (d3 : 'a3 decoder)
      (d4 : 'a4 decoder) (d5 : 'a5 decoder) (d6 : 'a6 decoder) :
      ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6) decoder =
    let* v1 = d1 in
    let* v2 = d2 in
    let* v3 = d3 in
    let* v4 = d4 in
    let* v5 = d5 in
    let+ v6 = d6 in
    (v1, v2, v3, v4, v5, v6)

  let tuple7 (d1 : 'a1 decoder) (d2 : 'a2 decoder) (d3 : 'a3 decoder)
      (d4 : 'a4 decoder) (d5 : 'a5 decoder) (d6 : 'a6 decoder)
      (d7 : 'a7 decoder) : ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7) decoder =
    let* v1 = d1 in
    let* v2 = d2 in
    let* v3 = d3 in
    let* v4 = d4 in
    let* v5 = d5 in
    let* v6 = d6 in
    let+ v7 = d7 in
    (v1, v2, v3, v4, v5, v6, v7)

  let tuple8 (d1 : 'a1 decoder) (d2 : 'a2 decoder) (d3 : 'a3 decoder)
      (d4 : 'a4 decoder) (d5 : 'a5 decoder) (d6 : 'a6 decoder)
      (d7 : 'a7 decoder) (d8 : 'a8 decoder) :
      ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8) decoder =
    let* v1 = d1 in
    let* v2 = d2 in
    let* v3 = d3 in
    let* v4 = d4 in
    let* v5 = d5 in
    let* v6 = d6 in
    let* v7 = d7 in
    let+ v8 = d8 in
    (v1, v2, v3, v4, v5, v6, v7, v8)

  let raw : sexp decoder = function
    | [] -> assert false
    | [] :: ((next::_)::_) -> Err next
    | [] :: _ -> GenErr
    | (v :: vs) :: next -> SomeRes (vs :: next, v)

  let%test _ = run raw (Atom "foo") = Some (Atom "foo")
  let%test _ = run raw (List []) = Some (List [])
  let%test _ = run raw (List [ Atom "foo" ]) = Some (List [ Atom "foo" ])
  let%test _ = run_list raw [ Atom "foo" ] = Some (Atom "foo")
  let%test _ = run_list raw [ Atom "foo"; Atom "bar" ] = None
  let%test _ = run_list raw [ List [] ] = Some (List [])
  let%test _ = run_list raw [ List [ Atom "foo" ] ] = Some (List [ Atom "foo" ])

  let skip : unit decoder = function
    | [] -> assert false
    | [] :: ((next::_)::_) -> Err next
    | [] :: _ -> GenErr
    | (_ :: vs) :: next -> SomeRes (vs :: next, ())

  let%test _ = run skip (Atom "foo") = Some ()
  let%test _ = run skip (List []) = Some ()
  let%test _ = run skip (List [ Atom "foo" ]) = Some ()
  let%test _ = run_list skip [ Atom "foo" ] = Some ()
  let%test _ = run_list skip [ Atom "foo"; Atom "bar" ] = None
  let%test _ = run_list (skip >>> skip) [ Atom "foo" ] = None
  let%test _ = run_list (skip >>> skip) [ Atom "foo"; Atom "bar" ] = Some ()
  let%test _ = run_list (raw >>> skip) [ Atom "foo"; Atom "bar" ] = Some ()

  let%test _ =
    run_list (raw <<< skip) [ Atom "foo"; Atom "bar" ] = Some (Atom "foo")

  let%test _ =
    run_list (raw >>> raw) [ Atom "foo"; Atom "bar" ] = Some (Atom "bar")

  let%test _ = run_list skip [ List [] ] = Some ()
  let%test _ = run_list skip [ List [ Atom "foo" ] ] = Some ()

  let atom : string decoder = function
    | [] -> assert false
    | [] :: ((next::_)::_) -> Err next
    | [] :: _ -> GenErr
    | ((List _ as err_point):: _) :: _ -> Err err_point
    | (Atom s :: vs) :: next -> SomeRes (vs :: next, s)

  let%test _ = run atom (Atom "foo") = Some "foo"
  let%test _ = run atom (List []) = None
  let%test _ = run atom (List [ Atom "foo" ]) = None
  let%test _ = run_list atom [ Atom "foo" ] = Some "foo"
  let%test _ = run_list atom [ Atom "foo"; Atom "bar" ] = None
  let%test _ = run_list (atom >>> skip) [ Atom "foo"; Atom "bar" ] = Some ()
  let%test _ = run_list (atom <<< skip) [ Atom "foo"; Atom "bar" ] = Some "foo"
  let%test _ = run_list (atom >>> atom) [ Atom "foo"; Atom "bar" ] = Some "bar"
  let%test _ = run_list atom [ List [] ] = None
  let%test _ = run_list atom [ List [ Atom "foo" ] ] = None

  let peek (d : 'a decoder) : 'a option decoder =
   fun state ->
    match d state with
    | SomeRes (_state, v) -> SomeRes (state, Some v)
    | GenErr | Err _ -> SomeRes (state, None)

  let%test _ = run_list (peek atom) [] = Some None
  let%test _ = run_list (peek atom) [ Atom "foo" ] = None
  let%test _ = run_list (peek atom <<< skip) [ Atom "foo" ] = Some (Some "foo")
  let%test _ = run_list (peek atom) [ List [] ] = None
  let%test _ = run_list (peek atom <<< skip) [ List [] ] = Some None

  let enter : unit decoder = function
    | [] -> assert false
    | ((Atom _ as err_point):: _) :: _ -> Err err_point
    | [] :: ((next::_)::_) -> Err next
    | [] :: _ -> GenErr
    | (List l :: vs) :: next -> SomeRes (l :: vs :: next, ())

  let exit : unit decoder = function
    | [] -> assert false
    | (err_point :: _) :: _ -> Err err_point
    | [] :: next -> SomeRes (next, ())

  let group (decode : 'a decoder) = enter >>> decode <<< exit

  let%test _ = run (group (return ())) (List []) = Some ()
  let%test _ = run (group (return ())) (List [ Atom "foo" ]) = None
  let%test _ = run (group atom) (List [ Atom "foo" ]) = Some "foo"
  let%test _ = run (group (return ())) (Atom "foo") = None
  let%test _ = run (group atom) (Atom "foo") = None

  let no_more : unit decoder = function
    | [] -> assert false
    | [] :: _ as state -> SomeRes (state, ())
    | (err_point :: _) :: _ -> Err err_point

  let%test _ = run (group no_more) (List []) = Some ()
  let%test _ = run (group no_more) (List [ Atom "foo" ]) = None
  let%test _ = run (group (atom <<< no_more)) (List [ Atom "foo" ]) = Some "foo"

  let filter (p : 'a -> bool) (decode : 'a decoder) : 'a decoder =
    let* v = decode in
    if p v then return v else error

  let tag s : string decoder = filter (String.equal s) atom

  let%test _ = run (tag "A") (Atom "A") = Some "A"
  let%test _ = run (tag "A") (Atom "B") = None

  let%test _ =
    run_list (tag "A" <*> tag "B") [ Atom "A"; Atom "B" ] = Some ("A", "B")

  let maybe (decode : 'a decoder) : 'a option decoder =
    (let+ v = decode in
     Some v)
    |+> return None

  let%test _ = run_list (maybe (tag "A")) [ Atom "A" ] = Some (Some "A")
  let%test _ = run_list (maybe (tag "A")) [ Atom "B" ] = None

  let%test _ =
    run_list (maybe (tag "A") <*> maybe (tag "B")) [ Atom "A" ]
    = Some (Some "A", None)

  let%test _ =
    run_list (maybe (tag "A") <*> maybe (tag "B")) [ Atom "B" ]
    = Some (None, Some "B")

  let maybe_with_default (default : 'a) (decode : 'a decoder) : 'a decoder =
    decode |+> return default

  let rec fix (f : (unit -> 'a decoder) -> 'a decoder) : 'a decoder =
    f (fun () -> fix f)

  let repeat_list ~(until : unit decoder) (decode : 'a decoder) :
      'a list decoder =
    fix (fun repeat_list_decode ->
        until >>> return [] |+>> fun () ->
        let+ v, l = decode <*> repeat_list_decode () in
        v :: l)

  let%test _ = run_list (repeat_list ~until:no_more (tag "A")) [] = Some []

  let%test _ =
    run_list (repeat_list ~until:no_more (tag "A")) [ Atom "A" ] = Some [ "A" ]

  let%test _ =
    run_list (repeat_list ~until:no_more (tag "A")) [ Atom "A"; Atom "A" ]
    = Some [ "A"; "A" ]

  let%test _ =
    run_list (repeat_list ~until:no_more (tag "A")) [ Atom "A"; Atom "B" ]
    = None

  let%test _ =
    run_list
      (repeat_list ~until:(drop (tag "B")) (tag "A"))
      [ Atom "A"; Atom "B" ]
    = Some [ "A" ]

  let%test _ =
    run_list
      (repeat_list ~until:no_more (tag "A" |+> tag "B"))
      [ Atom "A"; Atom "B" ]
    = Some [ "A"; "B" ]

  let%test _ =
    run_list
      (repeat_list ~until:no_more (tag "A" |+> (skip >>| fun () -> "not_A")))
      [ Atom "A"; Atom "B" ]
    = Some [ "A"; "not_A" ]

  let%test _ =
    run_list (repeat_list ~until:no_more skip) [ Atom "A"; List []; Atom "B" ]
    = Some [ (); (); () ]

  let%test _ =
    run_list
      (repeat_list ~until:(fail (tag "A")) (tag "A")
      <*> repeat_list ~until:no_more (tag "B"))
      [ Atom "A"; Atom "A"; Atom "B" ]
    = Some ([ "A"; "A" ], [ "B" ])

  let%test _ =
    run_list
      (repeat_list ~until:(fail (tag "A")) (tag "A")
      <*> repeat_list ~until:no_more (tag "B"))
      [ Atom "A"; Atom "A"; Atom "C"; Atom "B" ]
    = None

  let%test _ =
    run_list
      (repeat_list ~until:(fail (tag "A")) (tag "A")
      <*> repeat_list ~until:no_more (tag "B"))
      [ Atom "A"; Atom "A"; Atom "C" ]
    = None

  let repeat1_list ~(until : unit decoder) (decode : 'a decoder) :
      'a list decoder =
    let* v = decode in
    let+ l = repeat_list ~until decode in
    v :: l

  let repeat_full_list (decode : 'a decoder) : 'a list decoder =
    repeat_list ~until:no_more decode

  let repeat1_full_list (decode : 'a decoder) : 'a list decoder =
    repeat1_list ~until:no_more decode

  let repeat ~(until : unit decoder) (decode : unit decoder) : unit decoder =
    fix (fun repeat_decode -> until |+>> fun () -> decode >>> repeat_decode ())

  let repeat1 ~(until : unit decoder) (decode : unit decoder) : unit decoder =
    decode >>> repeat ~until decode

  let skip_all : unit decoder = repeat ~until:no_more skip

  let%test _ = run_list skip_all [ Atom "A"; List []; Atom "B" ] = Some ()
  let%test _ = run_list skip_all [] = Some ()

  let repeat_fold_left ~(until : unit decoder) ~(init : 'a)
      (decode : ('a -> 'a) decoder) : 'a decoder =
    let+ k =
      fix (fun repeat_fold ->
          until >>> return Fun.id |+>> fun () ->
          let* f = decode in
          let+ next = repeat_fold () in
          fun res -> next (f res))
    in
    k init

  let%test _ =
    run_list
      (repeat_fold_left ~until:no_more ~init:[] (atom >>| fun x xs -> x :: xs))
      []
    = Some []

  let%test _ =
    run_list
      (repeat_fold_left ~until:no_more ~init:[] (atom >>| fun x xs -> x :: xs))
      [ Atom "A"; Atom "B"; Atom "C" ]
    = Some [ "C"; "B"; "A" ]

  let repeat_fold_right ~(until : unit decoder) ~(init : 'a)
      (decode : ('a -> 'a) decoder) : 'a decoder =
    fix (fun repeat_fold ->
        until >>> return init |+>> fun () ->
        let* f = decode in
        let+ v = repeat_fold () in
        f v)

  let%test _ =
    run_list
      (repeat_fold_right ~until:no_more ~init:[] (atom >>| fun x xs -> x :: xs))
      []
    = Some []

  let%test _ =
    run_list
      (repeat_fold_right ~until:no_more ~init:[] (atom >>| fun x xs -> x :: xs))
      [ Atom "A"; Atom "B"; Atom "C" ]
    = Some [ "A"; "B"; "C" ]

  let bool : bool decoder =
    let* s = atom in
    match s with "true" -> return true | "false" -> return false | _ -> error

  let%test _ = run bool (Atom "true") = Some true
  let%test _ = run bool (Atom "false") = Some false
  let%test _ = run bool (Atom "foo") = None
  let%test _ = run bool (List []) = None

  let int : int decoder =
    let* s = atom in
    match int_of_string_opt s with Some n -> return n | None -> error

  let%test _ = run int (Atom "42") = Some 42
  let%test _ = run int (Atom "-127") = Some (-127)
  let%test _ = run int (Atom "foo") = None
  let%test _ = run int (List []) = None

  let float : float decoder =
    let* s = atom in
    match float_of_string_opt s with Some n -> return n | None -> error

  let%test _ = run float (Atom "42") = Some 42.
  let%test _ = run float (Atom "-127") = Some (-127.)
  let%test _ = run float (Atom "1.732") = Some 1.732
  let%test _ = run float (Atom "1.42e-3") = Some 0.00142
  let%test _ = run float (Atom "1.42e4") = Some 14200.00
  let%test _ = run float (Atom "foo") = None
  let%test _ = run float (List []) = None

  let string ?(escaped = false) : string decoder =
    let* s = atom in
    if not escaped then return s
    else
      match Scanf.unescaped s with
      | s' -> return s'
      | exception Scanf.Scan_failure _ -> error

  let%test _ = run string (Atom "ok") = Some "ok"
  let%test _ = run (string ~escaped:true) (Atom "ok") = Some "ok"
  let%test _ = run string (Atom "\"ok") = Some "\"ok"
  let%test _ = run (string ~escaped:true) (Atom "\"ok") = None
  let%test _ = run (string ~escaped:true) (Atom "\\\"ok") = Some "\"ok"

  let option (decode : 'a decoder) : 'a option decoder =
    group (decode >>| (fun v -> Some v) |+> return None)

  let%test _ = run (option (tag "A")) (List []) = Some None
  let%test _ = run (option (tag "A")) (Atom "A") = None
  let%test _ = run (option (tag "A")) (List [ Atom "A" ]) = Some (Some "A")
  let%test _ = run (option (tag "A")) (List [ Atom "A"; Atom "B" ]) = None
  let%test _ = run (option (tag "A")) (List [ Atom "B" ]) = None

  let list1 (decode : 'a decoder) : 'a list decoder =
    group (repeat1_full_list decode)

  let list (decode : 'a decoder) : 'a list decoder =
    group (repeat_full_list decode)

  let%test _ = run (list (tag "A")) (List []) = Some []
  let%test _ = run (list (tag "A")) (Atom "A") = None
  let%test _ = run (list (tag "A")) (List [ Atom "A" ]) = Some [ "A" ]

  let%test _ =
    run (list (tag "A")) (List [ Atom "A"; Atom "A" ]) = Some [ "A"; "A" ]

  let%test _ = run (list (tag "A")) (List [ Atom "A"; Atom "B" ]) = None

  let field name (decode : 'a decoder) : 'a decoder = group (tag name >>> decode)

  let%test _ =
    run (field "A" (int <*> bool)) (List [ Atom "A"; Atom "42"; Atom "true" ])
    = Some (42, true)

  let%test _ =
    run (field "A" (int <*> bool)) (List [ Atom "B"; Atom "42"; Atom "true" ])
    = None

  let%test _ =
    run (field "A" (int <*> bool)) (List [ Atom "A"; Atom "4.2"; Atom "true" ])
    = None

  let variant (cases : (string * 'a decoder) list) : 'a decoder =
    group
    @@ let* name = atom in
       match List.assoc_opt name cases with Some d -> d | None -> error

  let%test _ =
    run
      (variant
         [ ("A", bool >>| fun b -> `Bool b); ("B", int >>| fun n -> `Int n) ])
      (List [ Atom "A"; Atom "false" ])
    = Some (`Bool false)

  let%test _ =
    run
      (variant
         [ ("A", bool >>| fun b -> `Bool b); ("B", int >>| fun n -> `Int n) ])
      (List [ Atom "B"; Atom "42" ])
    = Some (`Int 42)

  let%test _ =
    run
      (variant
         [ ("A", bool >>| fun b -> `Bool b); ("B", int >>| fun n -> `Int n) ])
      (List [ Atom "A"; Atom "42" ])
    = None

  let%test _ =
    run
      (variant
         [ ("A", bool >>| fun b -> `Bool b); ("B", int >>| fun n -> `Int n) ])
      (List [ Atom "A"; Atom "false"; Atom "false" ])
    = None

  let%test _ =
    run
      (variant
         [ ("A", bool >>| fun b -> `Bool b); ("B", int >>| fun n -> `Int n) ])
      (List [ Atom "A" ])
    = None

  let%test _ =
    run
      (variant
         [ ("A", bool >>| fun b -> `Bool b); ("B", int >>| fun n -> `Int n) ])
      (List [ Atom "C"; Atom "42" ])
    = None

  let%test _ = run (variant []) (List [ Atom "C"; Atom "42" ]) = None
  let%test _ = run (variant []) (List []) = None

  let fields ~(default : 'a) (fs : (string * ('a -> 'a) decoder) list) :
      'a decoder =
    let dfield =
      group
      @@ let* name = atom in
         match List.assoc_opt name fs with Some d -> d | None -> error
    in
    repeat_fold_left ~init:default ~until:no_more dfield

  let record ~(default : 'a) (fs : (string * ('a -> 'a) decoder) list) :
      'a decoder =
    group @@ fields ~default fs

  let%test _ = run (record ~default:1 []) (List []) = Some 1

  let%test _ =
    run (record ~default:1 [ ("twice", return (( * ) 2)) ]) (List []) = Some 1

  let%test _ =
    run
      (record ~default:1 [ ("twice", return (( * ) 2)) ])
      (List [ List [ Atom "twice" ] ])
    = Some 2

  let%test _ =
    run
      (record ~default:1 [ ("twice", return (( * ) 2)) ])
      (List [ List [ Atom "twice" ]; List [ Atom "twice" ] ])
    = Some 4

  let%test _ =
    run
      (record ~default:1
         [ ("twice", return (( * ) 2)); ("add", int >>| fun n -> ( + ) n) ])
      (List [ List [ Atom "twice" ]; List [ Atom "twice" ] ])
    = Some 4

  let%test _ =
    run
      (record ~default:1
         [ ("twice", return (( * ) 2)); ("add", int >>| fun n -> ( + ) n) ])
      (List
         [
           List [ Atom "twice" ];
           List [ Atom "twice" ];
           List [ Atom "add"; Atom "1" ];
         ])
    = Some 5

  let%test _ =
    run
      (record ~default:1
         [ ("twice", return (( * ) 2)); ("add", int >>| fun n -> ( + ) n) ])
      (List
         [
           List [ Atom "twice" ];
           List [ Atom "add"; Atom "1" ];
           List [ Atom "twice" ];
         ])
    = Some 6

  let%test _ =
    run
      (record ~default:1
         [ ("twice", return (( * ) 2)); ("add", int >>| fun n -> ( + ) n) ])
      (List
         [
           List [ Atom "twice" ];
           List [ Atom "add"; Atom "1" ];
           Atom "twice";
           List [ Atom "twice" ];
         ])
    = None

  let%test _ =
    run
      (record ~default:1
         [ ("twice", return (( * ) 2)); ("add", int >>| fun n -> ( + ) n) ])
      (List
         [
           List [ Atom "twice" ];
           List [ Atom "add"; Atom "1" ];
           List [ Atom "foo" ];
           List [ Atom "twice" ];
         ])
    = None

  module Strings = Set.Make (String)

  let fields_advanced ~(default : 'a)
      (fs : (string * [< `Required | `Unique ] list * ('a -> 'a) decoder) list)
      : 'a decoder =
    let fs, required, unique =
      List.fold_right
        (fun (name, opts, d) (fs, required, unique) ->
          let required, unique =
            List.fold_left
              (fun (required, unique) -> function
                | `Required -> (Strings.add name required, unique)
                | `Unique -> (required, Strings.add name unique))
              (required, unique) opts
          in
          ((name, d) :: fs, required, unique))
        fs
        ([], Strings.empty, Strings.empty)
    in
    let dfield : ((Strings.t * 'a) option -> (Strings.t * 'a) option) decoder =
      group
      @@ let* name = atom in
         match List.assoc_opt name fs with
         | Some d -> (
             let+ f = d in
             function
             | None -> None
             | Some (encountered, res) ->
                 if Strings.mem name unique && Strings.mem name encountered then
                   None
                 else Some (Strings.add name encountered, f res))
         | None -> error
    in
    let* o_res =
      repeat_fold_left
        ~init:(Some (Strings.empty, default))
        ~until:no_more dfield
    in
    match o_res with
    | None -> error
    | Some (encountered, res) ->
        if Strings.subset required encountered then return res else error

  let record_advanced ~(default : 'a)
      (fs : (string * [< `Required | `Unique ] list * ('a -> 'a) decoder) list)
      : 'a decoder =
    group @@ fields_advanced ~default fs

  let%test _ =
    run
      (record_advanced ~default:1
         [
           ("twice", [], return (( * ) 2)); ("add", [], int >>| fun n -> ( + ) n);
         ])
      (List
         [
           List [ Atom "twice" ];
           List [ Atom "twice" ];
           List [ Atom "add"; Atom "1" ];
         ])
    = Some 5

  let%test _ =
    run
      (record_advanced ~default:1
         [
           ("twice", [], return (( * ) 2));
           ("add", [ `Required ], int >>| fun n -> ( + ) n);
         ])
      (List
         [
           List [ Atom "twice" ];
           List [ Atom "twice" ];
           List [ Atom "add"; Atom "1" ];
         ])
    = Some 5

  let%test _ =
    run
      (record_advanced ~default:1
         [
           ("twice", [ `Unique ], return (( * ) 2));
           ("add", [], int >>| fun n -> ( + ) n);
         ])
      (List
         [
           List [ Atom "twice" ];
           List [ Atom "twice" ];
           List [ Atom "add"; Atom "1" ];
         ])
    = None

  let%test _ =
    run
      (record_advanced ~default:1
         [
           ("twice", [], return (( * ) 2));
           ("add", [ `Required ], int >>| fun n -> ( + ) n);
         ])
      (List [ List [ Atom "twice" ]; List [ Atom "twice" ] ])
    = None

  let%test _ =
    run
      (record_advanced ~default:1
         [
           ("twice", [], return (( * ) 2));
           ("add", [ `Required ], int >>| fun n -> ( + ) n);
         ])
      (List
         [
           List [ Atom "twice" ];
           List [ Atom "twice" ];
           List [ Atom "add"; Atom "1" ];
           List [ Atom "add"; Atom "7" ];
         ])
    = Some 12

  let%test _ =
    run
      (record_advanced ~default:1
         [
           ("twice", [], return (( * ) 2));
           ("add", [ `Required; `Unique ], int >>| fun n -> ( + ) n);
         ])
      (List
         [
           List [ Atom "twice" ];
           List [ Atom "twice" ];
           List [ Atom "add"; Atom "1" ];
           List [ Atom "add"; Atom "7" ];
         ])
    = None

  let%test _ =
    (* A larger example, following a discussion that started here:
       https://discuss.ocaml.org/t/combinator-library-for-extracting-data-for-s-exps/10153/36
    *)
    let module M = struct
      type coords = float * float
      type radius = { at : coords; length : float; angles : float * float }
      type stroke = { width : float }
      type fill = { type_ : int option }

      type arc_args = {
        start : coords;
        end_ : coords;
        radius : radius;
        stroke : stroke;
        fill : fill;
      }

      type circle_args = {
        center : coords;
        radius : float;
        stroke : stroke;
        fill : fill;
      }

      type polyline_args = { pts : coords list; stroke : stroke; fill : fill }

      type object_ =
        | Arc of arc_args
        | Circle of circle_args
        | Polyline of polyline_args

      type symbol = { id : string; objects : object_ list }
    end in
    let open M in
    let d =
      let coords = float <*> float in
      let radius =
        let* at = field "at" coords in
        let* length = field "length" float in
        let+ angles = field "angles" (float <*> float) in
        { at; length; angles }
      in
      let stroke =
        let+ width = field "width" float in
        { width }
      in
      let fill =
        let+ type_ = field "type" @@ (tag "none" >>> return None) in
        { type_ }
      in
      let object_ =
        variant
          [
            ( "arc",
              fields
                ~default:
                  {
                    start = (0., 0.);
                    end_ = (0., 0.);
                    radius = { at = (0., 0.); length = 0.; angles = (0., 0.) };
                    stroke = { width = 0. };
                    fill = { type_ = None };
                  }
                [
                  ("start", coords >>| fun start args -> { args with start });
                  ("end", coords >>| fun end_ args -> { args with end_ });
                  ( "radius",
                    radius >>| fun radius (args : arc_args) ->
                    { args with radius } );
                  ( "stroke",
                    stroke >>| fun stroke (args : arc_args) ->
                    { args with stroke } );
                  ( "fill",
                    fill >>| fun fill (args : arc_args) -> { args with fill } );
                ]
              >>| fun args -> Arc args );
            ( "circle",
              fields
                ~default:
                  {
                    center = (0., 0.);
                    radius = 0.;
                    stroke = { width = 0. };
                    fill = { type_ = None };
                  }
                [
                  ("center", coords >>| fun center args -> { args with center });
                  ( "radius",
                    float >>| fun radius (args : circle_args) ->
                    { args with radius } );
                  ( "stroke",
                    stroke >>| fun stroke (args : circle_args) ->
                    { args with stroke } );
                  ( "fill",
                    fill >>| fun fill (args : circle_args) -> { args with fill }
                  );
                ]
              >>| fun args -> Circle args );
            ( "polyline",
              fields
                ~default:
                  { pts = []; stroke = { width = 0. }; fill = { type_ = None } }
                [
                  ( "pts",
                    repeat_full_list (field "xy" coords) >>| fun pts args ->
                    { args with pts } );
                  ( "stroke",
                    stroke >>| fun stroke (args : polyline_args) ->
                    { args with stroke } );
                  ( "fill",
                    fill >>| fun fill (args : polyline_args) ->
                    { args with fill } );
                ]
              >>| fun args -> Polyline args );
          ]
      in
      field "symbol"
      @@ let* id = string ~escaped:false in
         let+ objects = repeat_full_list object_ in
         { id; objects }
    in
    run d
      (List
         [
           Atom "symbol";
           Atom "\"Conn_Coaxial_0_1\"";
           List
             [
               Atom "arc";
               List [ Atom "start"; Atom "-1.778"; Atom "0.508" ];
               List [ Atom "end"; Atom "1.778"; Atom "0" ];
               List
                 [
                   Atom "radius";
                   List [ Atom "at"; Atom "-0.0508"; Atom "0" ];
                   List [ Atom "length"; Atom "1.8034" ];
                   List [ Atom "angles"; Atom "163.6"; Atom "0" ];
                 ];
               List [ Atom "stroke"; List [ Atom "width"; Atom "0.254" ] ];
               List [ Atom "fill"; List [ Atom "type"; Atom "none" ] ];
             ];
           List
             [
               Atom "arc";
               List [ Atom "start"; Atom "-1.778"; Atom "0.508" ];
               List [ Atom "end"; Atom "1.778"; Atom "0" ];
               List
                 [
                   Atom "radius";
                   List [ Atom "at"; Atom "-0.0254"; Atom "0" ];
                   List [ Atom "length"; Atom "1.8034" ];
                   List [ Atom "angles"; Atom "0"; Atom "-163.6" ];
                 ];
               List [ Atom "stroke"; List [ Atom "width"; Atom "0.254" ] ];
               List [ Atom "fill"; List [ Atom "type"; Atom "none" ] ];
             ];
           List
             [
               Atom "circle";
               List [ Atom "center"; Atom "0"; Atom "0" ];
               List [ Atom "radius"; Atom "0.508" ];
               List [ Atom "stroke"; List [ Atom "width"; Atom "0.2032" ] ];
               List [ Atom "fill"; List [ Atom "type"; Atom "none" ] ];
             ];
           List
             [
               Atom "polyline";
               List
                 [
                   Atom "pts";
                   List [ Atom "xy"; Atom "-2.54"; Atom "0" ];
                   List [ Atom "xy"; Atom "-0.508"; Atom "0" ];
                 ];
               List [ Atom "stroke"; List [ Atom "width"; Atom "0" ] ];
               List [ Atom "fill"; List [ Atom "type"; Atom "none" ] ];
             ];
           List
             [
               Atom "polyline";
               List
                 [
                   Atom "pts";
                   List [ Atom "xy"; Atom "0"; Atom "-2.54" ];
                   List [ Atom "xy"; Atom "0"; Atom "-1.778" ];
                 ];
               List [ Atom "stroke"; List [ Atom "width"; Atom "0" ] ];
               List [ Atom "fill"; List [ Atom "type"; Atom "none" ] ];
             ];
         ])
    = Some
        {
          id = "\"Conn_Coaxial_0_1\"";
          objects =
            [
              Arc
                {
                  start = (-1.778, 0.508);
                  end_ = (1.778, 0.);
                  radius =
                    {
                      at = (-0.0508, 0.);
                      length = 1.8034;
                      angles = (163.6, 0.);
                    };
                  stroke = { width = 0.254 };
                  fill = { type_ = None };
                };
              Arc
                {
                  start = (-1.778, 0.508);
                  end_ = (1.778, 0.);
                  radius =
                    {
                      at = (-0.0254, 0.);
                      length = 1.8034;
                      angles = (0., -163.6);
                    };
                  stroke = { width = 0.254 };
                  fill = { type_ = None };
                };
              Circle
                {
                  center = (0., 0.);
                  radius = 0.508;
                  stroke = { width = 0.2032 };
                  fill = { type_ = None };
                };
              Polyline
                {
                  pts = [ (-2.54, 0.); (-0.508, 0.) ];
                  stroke = { width = 0. };
                  fill = { type_ = None };
                };
              Polyline
                {
                  pts = [ (0., -2.54); (0., -1.778) ];
                  stroke = { width = 0. };
                  fill = { type_ = None };
                };
            ];
        }

  (* Example of partial decoding taken from
     https://gitlab.inria.fr/bmontagu/sexp_decode/-/issues/2 *)
  let%test _ =
    let some d = d >>| fun x -> Some x and none d = d >>> return None in
    let d =
      list
        (first
           [
             some @@ field "name" atom;
             some @@ field "impl" (list1 atom) >>| Option.map List.hd;
             none skip;
           ])
      >>| List.filter_map Fun.id
    in
    run d
    @@ List
         [
           List [ Atom "name"; Atom "Main" ];
           List [ Atom "intf"; List [] ];
           List [ Atom "impl"; List [ Atom "_build/default/src/bin/main.ml" ] ];
           List
             [
               Atom "cmt";
               List
                 [
                   Atom
                     "_build/default/src/bin/.main.eobjs/byte/dune__exe__Main.cmt";
                 ];
             ];
           List [ Atom "cmti"; List [] ];
           List
             [
               Atom "module_deps";
               List
                 [
                   List [ Atom "for_intf"; List [] ];
                   List [ Atom "for_impl"; List [] ];
                 ];
             ];
         ]
    = Some [ "Main"; "_build/default/src/bin/main.ml" ]

  (* Another example of partial decoding taken from
     https://gitlab.inria.fr/bmontagu/sexp_decode/-/issues/2 *)
  let%test _ =
    let unordered_field name decoder =
      let* li =
        first
          [
            field name decoder |> map (fun v -> Some v);
            skip |> map (fun () -> None);
          ]
        |> repeat_full_list
      in
      match List.filter_map Fun.id li with
      | [ v ] -> const v
      | [] -> error
      | _ :: _ :: _ ->
          (* multiple matching keys; ideally we would have a specific error message here *)
          error
    and pure d =
      let* result = peek d in
      match result with None -> error | Some v -> const v
    in
    let d =
      pair
        (pure @@ unordered_field "bar" raw)
        (pure @@ unordered_field "foo" raw)
      <<< repeat ~until:no_more skip
    in
    run_list d [ List [ Atom "foo"; Atom "0" ]; List [ Atom "bar"; Atom "1" ] ]
    = Some (Atom "1", Atom "0")
end

include Make (Csexp)
