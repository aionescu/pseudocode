module Compiler.Midend.Lowering

open Utils
open Compiler.Frontend.AST
open Compiler.Midend.IR

let panic () = failwith "Panic in Lowering"

let seq' = foldr (curry Seq) Nop

let rec lowerExpr (T (t, e)) =
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

      let setElem e = [Dup; lowerExpr e; Push <| ty e]
      seq' (NewList t :: List.collect setElem es)

  | Var i -> LoadVar i

  | Expr.Read e -> seq' [lowerExpr e; Write String; Read t]
  | Expr.Length e ->
      let t =
        match ty e with
        | List t -> Some t
        | _ -> None

      seq' [lowerExpr e; Length t]

  | Subscript (a, i) -> seq' [lowerExpr a; lowerExpr i; LoadIndex t]
  | Expr.Not e -> seq' [lowerExpr e; Not]
  | Expr.Negate e -> seq' [lowerExpr e; Negate]

  | Expr.Append (a, b) -> seq' [lowerExpr a; lowerExpr b; Append]
  | Expr.Pow (a, b) -> seq' [lowerExpr a; lowerExpr b; Pow]
  | Expr.Arith (op, a, b) -> seq' [lowerExpr a; lowerExpr b; Arith op]
  | Expr.Comp (op, a, b) -> seq' [lowerExpr a; lowerExpr b; Comp (op, t = String)]

  | Logic (And, a, b) -> seq' [lowerExpr a; If (lowerExpr b, PushBool false)]
  | Logic (Or, a, b) -> seq' [lowerExpr a; If (PushBool true, lowerExpr b)]

  | FnCall (f, args) -> seq' [seq' <| List.map lowerExpr args; Call f]

let rec lowerStmt stmt =
  match stmt with
  | Stmt.Let (_, None, _, _) -> panic ()
  | Stmt.Let (i, Some t, e, s) -> Let (i, t, lowerExpr e, lowerStmt s)

  | Assign (T (_, Var i), e) -> seq' [lowerExpr e; SetVar i]
  | Assign (T (t, Subscript (a, i)), e) -> seq' [lowerExpr a; lowerExpr i; lowerExpr e; SetIndex t]
  | Assign _ -> panic ()

  | Stmt.Push (i, es) ->
      let setElem e = [Dup; lowerExpr e; Push <| ty e]

      match List.splitAt (List.length es - 1) es with
      | es, [e] -> seq' <| lowerExpr i :: (List.collect setElem es @ [lowerExpr e; Push <| ty e])
      | _ -> panic ()

  | Stmt.Pop i ->
      match ty i with
      | List t -> seq' [lowerExpr i; Pop t]
      | _ -> panic ()

  | Stmt.Write es ->
      let writeSingle e = [lowerExpr e; Write <| ty e]
      seq' <| List.collect writeSingle es @ [WriteLine]

  | Stmt.If (c, t, e) -> seq' [lowerExpr c; If (lowerStmt t, lowerStmt e)]
  | Stmt.While (c, s) -> While (lowerExpr c, lowerStmt s)
  | Stmt.DoWhile (s, c) -> DoWhile (lowerStmt s, lowerExpr c)

  | Stmt.For (i, a, down, b, s) ->
      let comp, arith =
        if down then
          Gte, Sub
        else
          Lte, Add

      let cond = seq' [LoadVar i; lowerExpr b; Comp (comp, false)]
      let update = seq' [LoadVar i; PushInt 1; Arith arith; SetVar i]

      Let (i, Int, lowerExpr a, For (cond, lowerStmt s, update))

  | Stmt.Break -> Break
  | Stmt.Continue -> Continue

  | Stmt.Return None -> Return
  | Stmt.Return (Some e) -> seq' [lowerExpr e; Return]

  | FnCallStmt (f, args) -> seq' [seq' <| List.map lowerExpr args; Call f]

  | Stmt.Seq (a, b) -> seq' [lowerStmt a; lowerStmt b]
  | Stmt.Nop -> Nop

let lower p = mapFns (const' lowerStmt) p
