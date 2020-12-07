open Kicadsch.Sigs

type t = { left_inf: coord; right_sup: coord}

let create () =
  { left_inf= Coord(10000000000, 1000000000); right_sup = Coord(-100000, -100000)}

let create_from_rect (Coord (x, y) as c1)  (Coord (width, height)) =
  { left_inf= c1; right_sup= Coord(x+width, y+height)}

let create_from_limits (Coord (x1, y1)) (Coord (x2, y2)) =
  { left_inf= Coord ( (min x1 x2), (min y1 y2)); right_sup= Coord((max x1  x2),(max y1 y2))}

let add_rect {left_inf=Coord(xli_1, yli_1); right_sup=Coord(xrs_1, yrs_1)} {left_inf=Coord(xli_2, yli_2); right_sup=Coord(xrs_2, yrs_2)} =
  { left_inf= Coord ((min xli_1 xli_2), (min yli_1 yli_2)); right_sup = Coord((max xrs_1 xrs_2), (max yrs_1 yrs_2))}

let add_point {left_inf=Coord(xli, yli); right_sup=Coord(xrs, yrs) } (Coord(x, y)) =
  { left_inf= Coord ((min xli x), (min yli y)); right_sup = Coord((max xrs x), (max yrs y))}

let reformat ~min_size ~extend {left_inf=Coord(xli, yli); right_sup=Coord(xrs, yrs)} =
  let resize li rs = if (rs -li) < min_size then
      let middle = (rs + li ) / 2 in
      middle - min_size/2, middle + min_size/2
    else
      li - extend, rs + extend in
  let xmin, xmax = resize xli xrs and ymin, ymax = resize yli yrs in
  {left_inf=Coord(xmin, ymin); right_sup=Coord(xmax, ymax) }

let as_rect {left_inf=Coord(xli, yli) as c1; right_sup=Coord(xrs, yrs) } =
  c1, Coord (xrs - xli, yrs - yli)

let overlap_ratio {left_inf=Coord(xli_1, yli_1); right_sup=Coord(xrs_1, yrs_1)}  {left_inf=Coord(xli_2, yli_2); right_sup=Coord(xrs_2, yrs_2)} =
  let xli = max xli_1 xli_2 and yli = max yli_1 yli_2 and xrs = min xrs_1 xrs_2 and yrs = min yrs_1 yrs_2 in
  let intersected = (xli < xrs) && (yli < yrs) in
  if intersected then
    let surface = (xrs - xli)* (yrs - yli) and
    surface_1 = (xrs_1 - xli_1)* (yrs_1 - yli_1) and
    surface_2 = (xrs_2 - xli_2)* (yrs_2 - yli_2) in
    (float (max surface_1 surface_2) ) /. (float surface)
  else
    0.0

let compare {left_inf=Coord(xli_1, yli_1); right_sup=Coord(xrs_1, yrs_1)}  {left_inf=Coord(xli_2, yli_2); right_sup=Coord(xrs_2, yrs_2)} : int =
  let xli_r = xli_1 - xli_2 in
  if xli_r == 0 then
      let yli_r = yli_1 - yli_2 in
      if (yli_r == 0) then
          let xrs_r = xrs_1 - xrs_2 in
          if xrs_r == 0 then
            yrs_1 - yrs_2
          else
            xrs_r
      else
        yli_r
  else
    xli_r
