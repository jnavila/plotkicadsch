(** This function generates a parsing function which outputs an 'a
    option Note that some lines may not yield any correct output, so
    the output is an option. **)
let create_lib_parse_fun ~name ~regexp_str ~processing =
  let parser line =
    try Scanf.sscanf line regexp_str processing with
    | End_of_file ->
        Printf.printf "could not match %s (%s): line to short\n" name line ;
        None
    | Scanf.Scan_failure m ->
        Printf.printf "could not match %s (%s): %s\n" name line m ;
        None
  in
  parser

let create_parse_fun ~name ~regexp_str ~extract_fun =
  let parser line ~onerror ~process =
    try
      match Scanf.sscanf line regexp_str extract_fun with
      | None ->
          Printf.printf "Fields of %s could not be parsed (%s)\n" name line ;
          onerror ()
      | Some args ->
          process args
    with
    | End_of_file ->
        Printf.printf "could not match %s (%s): line to short\n" name line ;
        onerror ()
    | Scanf.Scan_failure m ->
        Printf.printf "could not match %s (%s): %s\n" name line m ;
        onerror ()
  in
  parser

let parse_list ?(cond = fun _ -> true) form s =
  let stream = Scanf.Scanning.from_string s in
  let rec do_parse acc =
    try
      let new_val = Scanf.bscanf stream form (fun x -> x) in
      if cond new_val then do_parse (new_val :: acc) else acc
    with
    | Scanf.Scan_failure _ ->
        acc
    | End_of_file ->
        acc
  in
  do_parse []
