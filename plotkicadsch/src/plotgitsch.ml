open StdLabels
open KicadDiff
open Cmdliner


let pp_fs out fs =
  Format.fprintf out "%s" (doc fs)

let get_fs s =
  if String.length s > 4 && String.equal (String.sub s ~pos:0 ~len:4) "dir:" then
    true_fs (String.sub s ~pos:4 ~len:(String.length s - 4))
  else git_fs s

let reference =
  let docv = "a commitish reference" in
  Arg.(conv ~docv ((fun s -> Result.Ok (get_fs s)), pp_fs))

let from_ref =
  let doc =
    "reference from which the diff is performed. If it starts with 'dir:' \
     it's a file system dir."
  in
  let docv = "FROM_REF" in
  Arg.(value & pos 0 reference (git_fs "HEAD") & info [] ~doc ~docv)

let to_ref =
  let doc =
    "target reference to diff with. If it starts with 'dir:' it's a file \
     system dir."
  in
  let docv = "TO_REF" in
  Arg.(value & pos 1 reference (true_fs ".") & info [] ~doc ~docv)

let pp_differ out differ =
  let s =
    match differ with
    | Internal p ->
        "internal with viewer " ^ p
    | Image_Diff ->
        "external"
  in
  Format.fprintf out "%s" s

let differ =
  let docv = "diff strategy used" in
  Arg.(conv ~docv ((fun d -> Result.Ok (Internal d)), pp_differ))

let diff_of_file =
  let doc = "diff only selected file $(docv)." in
  let docv = "FILENAME" in
  Arg.(value & opt (some file) None & info ["f"; "file"] ~doc ~docv)

let internal_diff =
  let doc =
    "use an internal diff algorithm and use the $(docv) to display the result."
  in
  let docv = "BROWSER" in
  let env = Arg.env_var ~doc:"Default viewer for internal diff. Defining this env var forces internal diff." "PLOTGITSCH_VIEWER" in
  Arg.(
    value
    & opt ~vopt:(Internal (SysAbst.default_opener ())) differ Image_Diff
    & info ["i"; "internal"] ~env ~doc ~docv)

let preloaded_libs =
  let doc =
    "preload symbol library $(docv) in order to prepare the diff. This option \
     can be used several times on command line."
  in
  let docv = "LIB" in
  Arg.(value & opt_all file [] & info ["l"; "lib"] ~doc ~docv)

let textual_diff =
  let doc =
    "fall back to show a text diff if files are different but generate no \
     visual diffs"
  in
  Arg.(value & flag & info ["t"; "textdiff"] ~doc)

let keep_files =
  let doc =
    "by default, the svg diff files are deleted after launching the viewer; \
     this option lets the files in place after viewing them. "
  in
  Arg.(value & flag & info ["k"; "keep"] ~doc)

let pp_colors out c =
  let open SvgPainter in
  match c with
  | None ->
      Format.fprintf out "default colors"
  | Some {old_ver; new_ver; fg; bg} ->
      Format.fprintf out "%s:%s:%s:%s" old_ver new_ver fg bg

let extract_colors s =
  let open SvgPainter in
  let col_exp = "([0-9a-fA-F]{6})" in
  let cols_exp =
    "^" ^ col_exp ^ ":" ^ col_exp ^ ":" ^ col_exp ^ ":" ^ col_exp ^ "$"
  in
  let col_re = Re.Posix.compile_pat cols_exp in
  match Re.all col_re s with
  | [m] -> (
    match Re.Group.all m with
    | [|_; o; n; f; b|] ->
        let e c = "#" ^ c in
        Result.Ok (Some {old_ver= e o; new_ver= e n; fg= e f; bg= e b})
    | _ ->
        Result.Error (`Msg "wrong colors format") )
  | _ ->
      Result.Error (`Msg "wrong colors format")

let get_colors =
  let docv = "scheme of colors for diffing" in
  Arg.(conv ~docv (extract_colors, pp_colors))

let colors =
  let doc =
    "list of colon separated hex RGB codes for colors used for diffing e.g. \
     the default colors are FF0000:00FF00:00000:FFFFFF"
  in
  let docv = "old:new:foreground:background" in
  let env = Arg.env_var ~doc:"Colors for plotting the diff" "PLOTGITSCH_COLORS" in

  Arg.(value & opt get_colors None & info ["c"; "colors"] ~env ~doc ~docv)

let pp_zone_color out c =
  match c with
  | None ->
    Format.fprintf out "transparent"
  | Some c ->
    Format.fprintf out "#%s" c

let extract_zone_color s =
  let col_exp = "(#[0-9a-fA-F]{6})" in
  let col_re = Re.Posix.compile_pat col_exp in
  match Re.all col_re s with
  | [_] -> Result.Ok (Some s)
  | _ ->
      Result.Error (`Msg "wrong colors format")

let get_zone_color =
  let docv = "RGB color format" in
  Arg.(conv ~docv (extract_zone_color, pp_zone_color))

let zone_color =
  let doc =
    "color of the frame around changed zones in hex RGB format, if specified"
  in
  let docv = "RGB, eg: #rrggbb" in
  let env = Arg.env_var ~doc:"Color for plotting frames around changes" "PLOTGITSCH_CHANGE_COLOR" in
  Arg.(value & opt get_zone_color None & info ["z"; "zone"] ~env ~doc ~docv)

let plotgitsch_t =
  Term.(
    const doit $ from_ref $ to_ref $ diff_of_file $ internal_diff
    $ textual_diff $ preloaded_libs $ keep_files $ colors $ zone_color)

let info =
  let doc =
    "Show graphically the differences between two git revisions of a kicad \
     schematic"
  in
  let man =
    [ `S Manpage.s_bugs
    ; `P "Open issues to https://github.com/jnavila/plotkicadsch/issues" ]
  in
  Term.info "plotgitsch" ~version:"%%VERSION%%" ~doc ~exits:Term.default_exits
    ~man

let () = Term.exit @@ Term.eval (plotgitsch_t, info)
