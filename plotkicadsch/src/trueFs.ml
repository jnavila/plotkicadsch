open Core_kernel
open Lwt.Infix
open DiffFs

let make rootname =
  ( module struct

    let label = TrueFS rootname

    let rootname = rootname

    let get_content filename =
      Lwt_io.with_file ~mode:Lwt_io.input
        (String.concat ~sep:Filename.dir_sep filename)
        Lwt_io.read

    let hash_file filename =
      get_content [filename]
      >|= fun c ->
      let blob_content = Printf.sprintf "blob %d\000" (String.length c) ^ c in
      (filename, Sha1.to_hex (Sha1.string blob_content))

    let list_files pattern =
      let all_files = Lwt_unix.files_of_directory rootname in
      let matched_files = Lwt_stream.filter pattern all_files in
      let decorated_files = Lwt_stream.map_s hash_file matched_files in
      Lwt_stream.to_list decorated_files
  end
  : Simple_FS )
