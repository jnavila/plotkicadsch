
type schContext

val initial_context : schContext

(**
   
**)  
val parse_line :
  schContext -> String.t -> schContext * [> `Polyline | `Text | `Svg ] Svg.M.elt option

(* For test only *)
val parse_F : String.t -> [> Svg_types.text ] Svg.M.elt option

val parse_wire_line : String.t -> [> Svg_types.polyline ] Svg.M.elt option
