open! StdLabels
open Kicadsch.Sigs

module type Differ = sig
  val doc : string

  type pctx

  module S : SchPainter with type painterContext = pctx

  val display_diff :
    from_ctx:pctx -> to_ctx:pctx -> string list -> keep:bool -> bool Lwt.t
end
