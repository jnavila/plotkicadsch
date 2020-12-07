open Kicadsch.Sigs

type t

val create: unit -> t

val create_from_rect: coord -> coord -> t

val create_from_limits: coord -> coord -> t

val add_rect: t -> t -> t

val add_point: t -> coord -> t

val reformat: min_size:int -> extend:int -> t -> t
val as_rect: t -> coord*coord

val overlap_ratio: t -> t -> float

val compare: t -> t -> int
