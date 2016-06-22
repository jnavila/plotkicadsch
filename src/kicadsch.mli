
type schContext

val initial_context : schContext

(**
   
**)  
val parse_line :
  schContext -> String.t -> schContext * [> `Polyline | `Text | `Svg | `Rect | `Circle ] Svg.M.elt option

(* For test only *)
val parse_F : ?context:int -> String.t -> [> Svg_types.text ] Svg.M.elt option

val parse_wire_line : ?context:int -> String.t -> [> Svg_types.polyline ] Svg.M.elt option
