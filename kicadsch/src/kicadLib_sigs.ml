open KicadSch_sigs

type relcoord = RelCoord of int * int

type circle = {center: relcoord; radius: int}

type pin_orientation = P_L | P_R | P_U | P_D

type pin_tag = string * size

type pin =
  { name: pin_tag
  ; number: pin_tag
  ; length: size
  ; contact: relcoord
  ; orient: pin_orientation
  }

type primitive =
  | Field
  | Polygon of int * relcoord list
  | Circle of int * circle
  | Pin of pin
  | Text of {c: relcoord; text: string; s: size}
  | Arc of
      { s: size
      ; radius: int
      ; sp: relcoord
      ; ep: relcoord
      ; center: relcoord
      }

type elt = {parts: int; prim: primitive}

type component =
  { names: string list
  ; draw_pnum: bool
  ; draw_pname: bool
  ; multi: bool
  ; graph: elt list
  }
