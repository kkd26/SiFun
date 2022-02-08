let handleExceptions e =
  match e with
  | Utils.LexBufException msg -> Printf.printf "\r%s" msg
  | Debruijn.DebruijnException msg -> Printf.printf "\rDebruijn Error:\n%s" msg
  | DBType.DBTypeException msg -> Printf.printf "\r%s" msg
  | Infer.InferException msg -> Printf.printf "\rInfer Error:\n%s" msg
  | Unify.UnifyException msg -> Printf.printf "\rUnify Error:\n%s" msg
  | _ -> ()
