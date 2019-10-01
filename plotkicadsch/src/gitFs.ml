open Core_kernel
open Lwt.Infix
open DiffFs
exception InternalGitError of string

let make commitish =
  ( module struct
    open Git_unix
    module Search = Git.Search.Make (Store)

    let rev_parse r =
      SysAbst.pread "git" [|"rev-parse"; r ^ "^{commit}"|]
      >>= fun s ->
      try Lwt.return @@ Store.Hash.of_hex @@ String.prefix s 40
      with _ -> Lwt.fail (InternalGitError ("cannot parse rev " ^ r))

    let label = GitFS commitish

    let git_root =
      let open Filename in
      let rec recurse (d, b) =
        let new_gitdir = concat d ".git/description" in
        try%lwt
          let%lwt _ = Lwt_unix.stat new_gitdir in
          (* that's a git repo and d is the root *)
          Lwt.return (d, b)
        with
        | Unix.Unix_error (Unix.ENOENT, _, _) ->
          let new_d = dirname d in
          if String.equal new_d d then
            (* we've reached the root of the FS *)
            Lwt.fail (InternalGitError "not in a git repository")
          else
            let new_b = basename d :: b in
            recurse (new_d, new_b)
        | e ->
          raise e
      in
      recurse @@ (Sys.getcwd (), [])

    let fs =
      let%lwt root, _ = git_root in
      match%lwt Store.v (Fpath.v root) with
      | Ok s ->
        Lwt.return s
      | Error e ->
        Lwt.fail (InternalGitError (Fmt.strf "%a" Store.pp_error e))

    let theref = rev_parse commitish

    let with_path path action =
      let%lwt t = fs in
      let%lwt h = theref in
      let%lwt _, rel_path = git_root in
      match%lwt
        Search.find t h (`Commit (`Path (List.concat [rel_path; path])))
      with
      | None ->
        Lwt.fail
          (InternalGitError
             ("path not found: /" ^ String.concat ~sep:Filename.dir_sep path))
      | Some sha -> (
          match%lwt Store.read t sha with
          | Ok a ->
            action a
          | Error e ->
            Lwt.fail (InternalGitError (Fmt.strf "%a" Store.pp_error e)) )

    let get_content filename =
      with_path filename
      @@ function
      | Store.Value.Blob b ->
        Lwt.return (Store.Value.Blob.to_string b)
      | _ ->
        Lwt.fail (InternalGitError "not a valid path")

    let find_file filter t =
      let open Store.Value.Tree in
      to_list t
      |> List.filter_map ~f:(fun {name; node; _} ->
          if filter name then Some (name, Store.Hash.to_hex node) else None
        )

    let list_files pattern =
      with_path []
      @@ function
      | Store.Value.Tree t ->
        Lwt.return @@ find_file pattern t
      | _ ->
        Lwt.fail (InternalGitError "not a tree!")
  end
  : Simple_FS )
