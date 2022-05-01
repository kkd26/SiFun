open DBType

exception ContextException of string

type cont = typeGenre -> typeGenre
type termCtx = int -> cont -> typeGenre

let emptyCtx : termCtx =
 fun i _k ->
  raise (ContextException ("Out of bounds variable " ^ string_of_int i))

let updateCtx (ctx : termCtx) (s : typeGenre) x (k : cont) =
  if x = 0 then k s else ctx (x - 1) k

let takeNth (i : int) (ctx : termCtx) : typeGenre = ctx i (fun x -> x)

let applyFunctionToContext (f : typeGenre -> typeGenre) (ctx : termCtx) :
    termCtx =
 fun i k -> ctx i (fun x -> k (f x))

let shift i c = applyFunctionToContext (shiftType i c)
