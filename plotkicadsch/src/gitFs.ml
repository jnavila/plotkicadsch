open StdLabels
open Lwt.Infix
open DiffFs
exception InternalGitError of string
exception PathNotFound of string list

let make commitish =
  ( module struct
    open Git_unix
    module Search = Git.Search.Make (Digestif.SHA1) (Store)
    let rev_parse r =
      SysAbst.pread "git" [|"rev-parse"; r ^ "^{commit}"|]
      >>= fun s ->
      try Lwt.return @@ Store.Hash.of_hex @@ String.sub ~pos:0 s ~len:(min 40 (String.length s))
      with _ -> Lwt.fail (InternalGitError ("cannot parse rev " ^ r))

    let label = GitFS commitish

    (* pair gitroot, relative_path where
       gitroot is the root dir of the git working copy
       relative_path is the actual path relative to gitroot*)
    let git_root =
      let open Filename in
      let rec recurse (d, b) =
        let new_gitdir = concat d ".git/description" in
        try%lwt
          let%lwt _ = Lwt_unix.stat new_gitdir in
          (* that's a git repo and d is the root *)
          Lwt.return (d, b)
        with
        | UnixLabels.Unix_error (UnixLabels.ENOENT, _, _) ->
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
          (PathNotFound path)
      | Some sha -> (
          match%lwt Store.read t sha with
          | Ok a ->
            action a
          | Error e ->
            Lwt.fail (InternalGitError (Fmt.strf "%a" Store.pp_error e)) )

    let get_content filename =
      try%lwt
        begin
          with_path filename
          @@ fun res -> match res with
          | Git.Value.Blob b ->
            Lwt.return (Store.Value.Blob.to_string b)
          | _ ->
            Lwt.fail (InternalGitError "not a valid path")
        end
      with
        PathNotFound _ -> Lwt.return ""

    let find_file_local filter (t: Store.Value.Tree.t) =
      let open Git.Tree in
      to_list t
      |> List.filter_map ~f:(fun t -> let {node; name; _} = t in
            if filter name then Some ([name], Store.Hash.to_hex node) else None
        )
    ;;

    let find_dir_local t =
      let open Git.Tree in
      to_list t
      |> List.filter ~f:(fun {perm;_} -> perm == `Dir)
    ;;

    let rec recurse_dir ?dirname node pattern =
      let rename name = match dirname with
        | Some dirname -> dirname::name
        | None -> name in
      let local_file_list = find_file_local pattern node in
      let path_file_list = List.map local_file_list ~f:(fun (name, hash) -> ((rename name), hash)) in
      let dirs = find_dir_local node in
      let%lwt t = fs in
      let open Git.Tree in
      let recurse_tree = fun entry ->
          let%lwt res  = Store.read t entry.node in
          match res with
          |Error e -> Lwt.fail (InternalGitError (Fmt.strf "%a" Store.pp_error e))
          |Ok Git.Value.Tree t ->(
            let%lwt subdir = recurse_dir ~dirname:entry.name t pattern in
            let subdir_files = List.map ~f:(fun (name, hash) -> ((rename name), hash)) subdir in
            Lwt.return subdir_files)
          |Ok _  -> Lwt.fail (InternalGitError ("impossible case")) in
      let%lwt subdir_list = Lwt_list.map_s recurse_tree dirs in
      let result = List.concat [List.concat subdir_list; path_file_list] in
      Lwt.return result

    let list_files_from path pattern =
      with_path path
      @@ function
      | Git.Value.Tree t -> recurse_dir t pattern
      | _ -> Lwt.fail (InternalGitError "not a tree!")

    let list_files pattern =list_files_from [] pattern
  end
  : Simple_FS )
