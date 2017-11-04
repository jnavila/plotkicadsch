(** This function generates a parsing function which outputs an 'a
    option Note that some lines may not yield any correct output, so
    the output is an option. **)
let create_lib_parse_fun ~name ~regexp_str ~processing =
  let regexp = Pcre.regexp regexp_str in
  let parser line =
    try
      let sp = Pcre.extract ~rex:regexp line in
      processing sp
    with Not_found ->
      (Printf.printf "could not match %s (%s)\n" name line; None)
  in parser

let create_parse_fun ~name ~regexp_str ~extract_fun =
  let regexp = Pcre.regexp regexp_str in
  let parser line ~onerror ~process  =
    try
      let sp = Pcre.extract ~rex:regexp line
      in
      match (extract_fun sp) with
      | None ->  (Printf.printf "Fields of %s could not be parsed (%s)\n" name line; onerror ())
      | Some args -> process args
    with Not_found ->
      (Printf.printf "could not match %s (%s)\n" name line; onerror ())
  in parser
