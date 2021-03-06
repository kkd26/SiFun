let handleExceptions e =
  match e with
  | Utils.LexBufException msg -> Printf.printf "\r%s" msg
  | DBAst.DBAstException msg -> Printf.printf "\rDBAst Error:\n%s" msg
  | DBType.DBTypeException msg -> Printf.printf "\r%s" msg
  | Infer.InferException msg -> Printf.printf "\rInfer Error:\n%s" msg
  | Unify.UnifyException msg -> Printf.printf "\rUnify Error:\n%s" msg
  | TermCtx.ContextException msg -> Printf.printf "\rContext Error:\n%s" msg
  | Failure msg -> Printf.printf "\rError:\n%s" msg
  | _ -> ()
