open KicadSch_sigs

module MakeSchPainter: functor (P: Painter) -> SchPainter with type painterContext := P.t
