open KicadDefs

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
  | Bezier of int * relcoord list

type elt = { parts: int
           ; prim: primitive
           }

type component =
  { names: string list
  ; draw_pnum: bool
  ; draw_pname: bool
  ; multi: bool
  ; graph: elt list
  }

module Lib : Hashtbl.S with type key := string = Hashtbl.Make (struct
    type t = string

    let equal = String.equal

    let get_i s n = int_of_char s.[n]

    let hash s =
      let rec build_hash h i =
        if i < 0 then h else build_hash ((h * 47) + get_i s i) (i - 1)
      in
      build_hash 0 (String.length s - 1)
  end)

type library = component Lib.t
let lib (): library = Lib.create 256

let fix_illegal_chars name =
  String.map (function '/' | ':' -> '_' | c -> c) name

let add_component comp lib =
  List.iter
    (fun name -> Lib.replace lib (fix_illegal_chars name) comp)
    comp.names ;
  lib


let get_comp lib comp_name =
        Lib.find_opt lib (fix_illegal_chars comp_name)
