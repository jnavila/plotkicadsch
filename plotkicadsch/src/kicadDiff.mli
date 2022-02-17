(** schematic diffing module *)

(** type of diffing. If internal, specify the application for showing SVGs **)
type differ = Internal of string | Image_Diff

type t
(** type of the file system for each leg of the diff *)

val git_fs : string -> t
(** [git_fs rev] builds a file system tree based on a git revision [rev] *)

val true_fs : string -> t
(** [true_fs root] builds a fs from the file system [root] directory *)

val doc : t -> string
(** [doc fs] outputs the doc string of the file system [fs] *)

val doit :
  t ->
  t ->
  string option ->
  differ ->
  bool ->
  string list ->
  bool ->
  SvgPainter.diff_colors option ->
  string option ->
  bool ->
  string option ->
  unit
(** [doit fs_from fs_to filename differ textdiff libs keep colors allow_missing relative_path]
    performs the diff of [filename] from [relative_path] if present between
    [fs_from] and [fs_to] using strategy [differ] and using common [libs] and
    [colors] scheme. If [textdiff], then a text diff is shown when no visual
    diff, if [keep] then the diff file isn't removed after *)
