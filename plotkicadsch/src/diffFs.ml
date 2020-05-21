
type t = TrueFS of string | GitFS of string

module type Simple_FS = sig

  val label : t

  val get_content : string list -> string Lwt.t

  val list_files : (string -> bool) -> (string list * string) list Lwt.t
end
