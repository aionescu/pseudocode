module Compiler.Midend.Simplifier

open Utils
open Compiler.Frontend.Syntax
open Compiler.Midend.Core

let panic () = failwith "Panic in Simplifier"

let seq' = foldr (curry Seq) Nop

let rec toList = function
  | Seq (a, b) -> toList a @ toList b
  | Nop -> []
  | i -> [i]

let rec toSeq = function
  | [] -> Nop
  | [a] -> a
  | a :: as' -> Seq (a, toSeq as')

let flatten = toList >> toSeq

let rec simplifyExpr (T (t, e)) =
  match e with
  | BoolLit b -> PushBool b
  | IntLit i -> PushInt i
  | FloatLit f  -> PushFloat f
  | StringLit s -> PushString s

  | ListLit es ->
      let t =
        match t with
        | List t -> t
        | _ -> panic ()

      let setElem e = [Dup; simplifyExpr e; Push <| ty e]
      seq' (NewList t :: List.collect setElem es)

  | Var i -> LoadVar i
  | Arg i -> LoadArg i

  | Expr.Read e -> seq' [simplifyExpr e; Write String; Read t]
  | Expr.Length e ->
      let t =
        match ty e with
        | List t -> Some t
        | _ -> None

      seq' [simplifyExpr e; Length t]

  | Subscript (a, i) -> seq' [simplifyExpr a; simplifyExpr i; LoadIndex t]
  | Expr.Not e -> seq' [simplifyExpr e; Not]
  | Expr.Negate e -> seq' [simplifyExpr e; Negate]

  | Expr.Append (a, b) -> seq' [simplifyExpr a; simplifyExpr b; Append]
  | Expr.Pow (a, b) -> seq' [simplifyExpr a; simplifyExpr b; Pow]
  | Expr.Arith (op, a, b) -> seq' [simplifyExpr a; simplifyExpr b; Arith op]
  | Expr.Comp (op, a, b) -> seq' [simplifyExpr a; simplifyExpr b; Comp (op, t = String)]

  | Logic (And, a, b) -> seq' [simplifyExpr a; If (simplifyExpr b, PushBool false)]
  | Logic (Or, a, b) -> seq' [simplifyExpr a; If (PushBool true, simplifyExpr b)]

  | FnCall (f, args) -> seq' [seq' <| List.map simplifyExpr args; Call f]

let rec simplifyStmt stmt =
  flatten <|
    match stmt with
    | Let (_, None, _, _) -> panic ()
    | Let (i, Some t, e, s) -> seq' [simplifyExpr e; SetVar i; simplifyStmt s; ClearVar (i, t)]

    | Assign (T (_, Var i), e) -> seq' [simplifyExpr e; SetVar i]
    | Assign (T (t, Subscript (a, i)), e) -> seq' [simplifyExpr a; simplifyExpr i; simplifyExpr e; SetIndex t]
    | Assign _ -> panic ()

    | Stmt.Push (i, es) ->
        let setElem e = [Dup; simplifyExpr e; Push <| ty e]

        match List.splitAt (List.length es - 1) es with
        | es, [e] -> seq' <| simplifyExpr i :: (List.collect setElem es @ [simplifyExpr e; Push <| ty e])
        | _ -> panic ()

    | Stmt.Pop i ->
        match ty i with
        | List t -> seq' [simplifyExpr i; Pop t]
        | _ -> panic ()

    | Stmt.Write es ->
        let writeSingle e = [simplifyExpr e; Write <| ty e]
        seq' <| List.collect writeSingle es @ [WriteLine]

    | Stmt.If (c, t, e) -> seq' [simplifyExpr c; If (simplifyStmt t, simplifyStmt e)]
    | Stmt.While (c, s) -> While (simplifyExpr c, simplifyStmt s)
    | Stmt.DoWhile (s, c) -> DoWhile (simplifyStmt s, simplifyExpr c)

    | Stmt.For (i, a, down, b, s) ->
        let comp, arith =
          if down then
            Gte, Sub
          else
            Lte, Add

        let cond = seq' [LoadVar i; simplifyExpr b; Comp (comp, false)]
        let update = seq' [LoadVar i; PushInt 1; Arith arith; SetVar i]
        seq' [simplifyExpr a; SetVar i; For (cond, simplifyStmt s, update)]

    | Stmt.Break -> Break
    | Stmt.Continue -> Continue

    | Stmt.Return None -> Return
    | Stmt.Return (Some e) -> seq' [simplifyExpr e; Return]

    | FnCallStmt (f, args) -> seq' [seq' <| List.map simplifyExpr args; Call f]

    | Stmt.Seq (a, b) -> seq' [simplifyStmt a; simplifyStmt b]
    | Stmt.Nop -> Nop

let simplify p = mapFns (const' <| second simplifyStmt) p
