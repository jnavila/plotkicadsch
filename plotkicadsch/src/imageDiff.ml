open! StdLabels
open Lwt.Infix

let doc = "use compare (ImageMagick) between bitmaps"

type pctx = SvgPainter.t

module SVG = Kicadsch.MakeSchPainter (SvgPainter)
module SP = struct
  include SVG

  type painterContext = SvgPainter.t
end

module S = SP

let display_diff ~from_ctx ~to_ctx filename ~keep =
  let from_filename = SysAbst.build_tmp_svg_name ~keep "from_" filename in
  let to_filename = SysAbst.build_tmp_svg_name ~keep "to_" filename in
  let both_files =
    List.map
      ~f:(fun (svg_name, context) ->
          Lwt_io.with_file ~mode:Lwt_io.Output svg_name (fun o ->
              Lwt_io.write o (SvgPainter.write context) ) )
      [(from_filename, from_ctx); (to_filename, to_ctx)]
  in
  let both = Lwt.join both_files in
  let compare_them =
    both
    >>= fun _ ->
    SysAbst.exec "git-imgdiff" [|from_filename; to_filename|]
    >|= let open UnixLabels in
    function
    | WEXITED ret ->
      if Int.equal ret 0 then true else false
    | WSIGNALED _ ->
      false
    | WSTOPPED _ ->
      false
  in
  let%lwt ret =
    try%lwt compare_them with
    | GitFs.InternalGitError s ->
      Lwt_io.printf "%s\n" s >|= fun () -> false
    | _ ->
      Lwt_io.printf "unknown error\n" >|= fun () -> false
  in
  Lwt.join
  @@ List.map
    ~f:(SysAbst.finalize_tmp_file ~keep)
    [from_filename; to_filename]
  >|= fun _ -> ret
