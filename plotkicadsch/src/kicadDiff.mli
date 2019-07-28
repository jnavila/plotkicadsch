type fs_type = TrueFS of string | GitFS of string

type differ = Internal of string | Image_Diff

module type Simple_FS = sig
  val doc : string

  val label : fs_type

  val get_content : string list -> string Lwt.t

  val list_files : (string -> bool) -> (string * string) list Lwt.t
end

val git_fs: string -> (module Simple_FS)

val true_fs: string -> (module Simple_FS)

val doit: (module Simple_FS) -> (module Simple_FS) ->
  string option ->
  differ ->
  bool -> string list -> bool -> SvgPainter.diff_colors option -> unit
