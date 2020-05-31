open Core_kernel
open Lwt.Infix
open DiffFs

let make rootname =
  ( module struct

    let label = TrueFS rootname

    let rootname = String.lstrip ~drop:(Char.equal '/') rootname
    let rootlength = (String.length rootname) + 1

    let get_content filename =
      Lwt_io.with_file ~mode:Lwt_io.input
        (String.concat ~sep:Filename.dir_sep filename)
        Lwt_io.read

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
         let filename = String.drop_prefix  filename rootlength in
         let file_path = String.split ~on:'/' filename in
         hash_file file_path) list in
     file_list
 end
  : Simple_FS )
