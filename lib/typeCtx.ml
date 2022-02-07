open DBType

type typeCtx = typeKind list

let emptyCtx : typeCtx = []
let updateCtx (ctx : typeCtx) (s : typeKind) = s :: ctx
let find (i : int) (ctx : typeCtx) : typeKind = List.nth ctx i
let shift i c = List.map (shiftType i c)
