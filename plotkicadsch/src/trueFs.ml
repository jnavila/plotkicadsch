open StdLabels
open Lwt.Infix
open DiffFs

let make rootname =
  ( module struct

    let label = TrueFS rootname

    let lstrip c s =
      let rec find_non_c c s n =
        if s.[n] != c then
          String.sub ~pos:n ~len:(String.length s - n) s
        else
          find_non_c c s (n+1)
      in
      find_non_c c s 0

    let rootname = lstrip '/' rootname
    let rootlength = (String.length rootname) + 1

    let get_content filename =
      try%lwt
        Lwt_io.with_file ~mode:Lwt_io.input
          (String.concat ~sep:Filename.dir_sep filename)
          Lwt_io.read
      with
        _ -> Lwt.return ""

    let hash_file filename =
      get_content filename
      >|= fun c ->
      let blob_content = Printf.sprintf "blob %d\000" (String.length c) ^ c in
      (filename, Sha1.to_hex (Sha1.string blob_content))

    let dir_contents dir pattern =
      let rec loop result = function
        | f::fs when Sys.is_directory f ->
          Sys.readdir f
          |> Array.to_list
          |> List.map ~f:(Filename.concat f)
          |> List.append fs
          |> loop result
        | f::fs when pattern f -> loop (f::result) fs
        | _::fs -> loop result fs
        | []    -> result
      in
      loop [] [dir]

   let list_files pattern =
     let list = dir_contents rootname pattern in
     let file_list = Lwt_list.map_s (fun filename ->
         let filename = String.sub filename ~pos:rootlength ~len:(String.length filename - rootlength) in
         let file_path = String.split_on_char ~sep:'/' filename in
         hash_file file_path) list in
     file_list
 end
  : Simple_FS )
