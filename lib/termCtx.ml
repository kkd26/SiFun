open DBType

type termCtx = typeGenre list

let emptyCtx : termCtx = []
let updateCtx (ctx : termCtx) (s : typeGenre) = s :: ctx
let takeNth (i : int) (ctx : termCtx) : typeGenre = List.nth ctx i
let shift i c = List.map (shiftType i c)
