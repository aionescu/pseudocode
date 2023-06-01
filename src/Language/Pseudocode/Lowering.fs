module Language.Pseudocode.Lowering

open Language.Pseudocode.Syntax
open Language.Pseudocode.IR

let panic () = failwith "Panic in Lowering"

let unList = function
  | List t -> t
  | _ -> panic ()

let rec lowerExpr args (T (t, e)) =
  match e with
  | BoolLit b -> [PushBool b]
  | IntLit i -> [PushInt i]
  | FloatLit f  -> [PushFloat f]
  | StringLit s -> [PushString s]

  | ListLit es ->
      let setElem e = Dup :: (lowerExpr args e @ [ListPush <| ty e])
      NewList (unList t) :: List.collect setElem es

  | Var i ->
      match List.tryFindIndex ((=) i) args with
      | None -> [LoadVar i]
      | Some idx -> [LoadArg idx]

  | Expr.Read e -> lowerExpr args e @ [Write String; Read t]
  | Length e ->
      lowerExpr args e @
        match ty e with
        | List t -> [ListLength t]
        | String -> [StrLength]
        | _ -> panic ()

  | Subscript (a, i) -> lowerExpr args a @ lowerExpr args i @ [LoadIndex t]
  | Expr.Not e -> lowerExpr args e @ [Not]
  | Expr.Negate e -> lowerExpr args e @ [Negate]

  | Expr.Append (a, b) -> lowerExpr args a @ lowerExpr args b @ [Append]
  | Expr.Pow (a, b) -> lowerExpr args a @ lowerExpr args b @ [Pow]
  | Expr.Arith (op, a, b) -> lowerExpr args a @ lowerExpr args b @ [Arith op]
  | Expr.Comp (op, a, b) -> lowerExpr args a @ lowerExpr args b @ [if t = String then StrComp op else Comp op]

  | Logic (And, a, b) -> lowerExpr args a @ [If (lowerExpr args b, [PushBool false])]
  | Logic (Or, a, b) -> lowerExpr args a @ [If ([PushBool true], lowerExpr args b)]

  | FnCall (f, a) -> List.collect (lowerExpr args) a @ [Call f]

let rec lowerStmt args stmt =
  match stmt with
  | Stmt.Let (_, None, _, _) -> panic ()
  | Stmt.Let (i, Some t, e, s) -> [Let (i, t, lowerExpr args e, lowerStmt args s)]

  | Assign (T (_, Var i), e) ->
      lowerExpr args e @
        match List.tryFindIndex ((=) i) args with
        | None -> [SetVar i]
        | Some idx -> [SetArg idx]

  | Assign (T (t, Subscript (a, i)), e) -> lowerExpr args a @ lowerExpr args i @ lowerExpr args e @ [SetIndex t]
  | Assign _ -> panic ()

  | Push (i, es) ->
      let setElem e = Dup :: lowerExpr args e @ [ListPush <| ty e]

      match List.splitAt (List.length es - 1) es with
      | es, [e] -> lowerExpr args i @ List.collect setElem es @ lowerExpr args e @ [ListPush <| ty e]
      | _ -> panic ()

  | Pop i -> lowerExpr args i @ [ListPop <| unList (ty i)]

  | Stmt.Write es ->
      let writeSingle e = lowerExpr args e @ [Write <| ty e]
      List.collect writeSingle es @ [WriteLine]

  | Stmt.If (c, t, e) -> lowerExpr args c @ [If (lowerStmt args t, lowerStmt args e)]
  | Stmt.While (c, s) -> [While (lowerExpr args c, lowerStmt args s)]
  | Stmt.DoWhile (s, c) -> [DoWhile (lowerStmt args s, lowerExpr args c)]
  | Stmt.For (i, a, down, b, s) -> [Let (i, Int, lowerExpr args a, [For (i, down, lowerExpr args b, lowerStmt args s)])]

  | Stmt.Break -> [Break]
  | Stmt.Continue -> [Continue]

  | Stmt.Return None -> [Return]
  | Stmt.Return (Some e) -> lowerExpr args e @ [Return]

  | FnCallStmt (f, a) -> List.collect (lowerExpr args) a @ [Call f]

  | Seq (a, b) -> lowerStmt args a @ lowerStmt args b
  | Nop -> []

let lowerFn fnSig = lowerStmt <| List.map fst fnSig.args

let lower p = mapFns lowerFn p
