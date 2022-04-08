open DBType

type termCtx = typeKind list

let emptyCtx : termCtx = []
let updateCtx (ctx : termCtx) (s : typeKind) = s :: ctx
let find (i : int) (ctx : termCtx) : typeKind = List.nth ctx i
let shift i c = List.map (shiftType i c)
