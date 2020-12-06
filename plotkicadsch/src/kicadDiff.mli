(**
   schematic diffing module *)

(** type of diffing. If internal, specify the application for showing SVGs **)
type differ = Internal of string | Image_Diff

(** type of the file system for each leg of the diff *)
type t

(** [git_fs rev] builds a file system tree based on a git revision [rev] *)
val git_fs: string -> t

(** [true_fs root] builds a fs from the file system [root] directory *)
val true_fs: string -> t

(** [doc fs] outputs the doc string of the file system [fs] *)
val doc: t -> string

(** [doit fs_from fs_to filename differ textdiff libs keep colors]
    performs the diff of [filename] between [fs_from] and [fs_to]
    using strategy [differ] and using common [libs] and [colors]
    scheme. If [textdiff], then a text diff is shown when no visual
    diff, if [keep] then the diff file isn't removed after *)
val doit: t -> t -> string option ->
  differ -> bool -> string list -> bool ->
  SvgPainter.diff_colors option -> string option -> unit
