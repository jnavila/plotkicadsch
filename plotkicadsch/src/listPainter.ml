open Kicadsch.Sigs

type image_data = Buffer.t

type t =
  | Text of kolor * string * orientation * coord * size * justify * style
  | Line of kolor * size * coord * coord
  | Rect of kolor * kolor * coord * coord
  | Circle of kolor * kolor * coord * int
  | Arc of kolor * kolor * coord * coord * coord * int
  | Image of coord * float * image_data
  | Format of coord
  | Zone of coord * coord

type listcanevas = t list

module L = struct
  type t = listcanevas

  type painterContext = listcanevas

  let paint_text ?(kolor = `Black) text (o : orientation) coords s j stl ctx =
    Text (kolor, text, o, coords, s, j, stl) :: ctx

  let paint_line ?(kolor = `Black) ?(width = Size 2) pt_start pt_end ctx =
    Line (kolor, width, pt_start, pt_end) :: ctx

  let paint_rect ?(kolor = `Black) ?(fill = `NoColor) pt dims ctx =
    Rect (kolor, fill, pt, dims) :: ctx

  let paint_circle ?(kolor = `Black) ?(fill = `NoColor) center radius ctx =
    Circle (kolor, fill, center, radius) :: ctx

  let paint_arc ?(kolor = `Black) ?(fill = `NoColor) pt_center pt_start pt_stop
      radius ctx =
    Arc (kolor, fill, pt_center, pt_start, pt_stop, radius) :: ctx

  let paint_image corner scale b c = Image (corner, scale, b) :: c


  let get_context () = []

  let set_canevas_size x y c = Format (Coord (x, y)) :: c
end
