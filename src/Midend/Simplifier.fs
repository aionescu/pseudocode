module Midend.Simplifier

open Utils.Misc
open Frontend.Syntax
open Midend.Core

let panic () = failwith "Panic in Simplifier"

let rec simplifyExpr (T (t, e)) =
  match e with
  | BoolLit b -> [PushBool b]
  | IntLit i -> [PushInt i]
  | FloatLit f  -> [PushFloat f]
  | StringLit s -> [PushString s]

  | ListLit es ->
      let t =
        match t with
        | List t -> t
        | _ -> panic ()

      let setElem e = [Dup] @ simplifyExpr e @ [Push <| ty e]
      [NewList t] @ List.collect setElem es

  | Var i -> [LoadVar i]

  | Expr.Read e -> simplifyExpr e @ [Write String; Read t]
  | Expr.Length e ->
      let t =
        match ty e with
        | List t -> Some t
        | _ -> None

      simplifyExpr e @ [Length t]

  | Subscript (a, i) -> simplifyExpr a @ simplifyExpr i @ [LoadIndex t]
  | Expr.Not e -> simplifyExpr e @ [Not]
  | Expr.Negate e -> simplifyExpr e @ [Negate]

  | Expr.Append (a, b) -> simplifyExpr a @ simplifyExpr b @ [Append]
  | Expr.Pow (a, b) -> simplifyExpr a @ simplifyExpr b @ [Pow]
  | Expr.Arith (op, a, b) -> simplifyExpr a @ simplifyExpr b @ [Arith op]
  | Expr.Comp (op, a, b) -> simplifyExpr a @ simplifyExpr b @ [Comp (op, t = String)]

  | Logic (And, a, b) -> simplifyExpr a @ [If (simplifyExpr b, [PushBool false])]
  | Logic (Or, a, b) -> simplifyExpr a @ [If ([PushBool true], simplifyExpr b)]

let rec simplifyStmt stmt =
  match stmt with
  | Let (_, None, _, _) -> panic ()
  | Let (i, Some t, e, s) -> simplifyExpr e @ [SetVar i] @ simplifyStmt s @ [ClearVar (i, t)]

  | Assign (T (_, Var i), e) -> simplifyExpr e @ [SetVar i]
  | Assign (T (t, Subscript (a, i)), e) ->
      simplifyExpr a @ simplifyExpr i @ simplifyExpr e @ [SetIndex t]
  | Assign _ -> panic ()

  | Stmt.Push (i, es) ->
      let setElem e = [Dup] @ simplifyExpr e @ [Push <| ty e]

      match List.splitAt (List.length es - 1) es with
      | es, [e] ->
          simplifyExpr i
          @ List.collect setElem es
          @ simplifyExpr e
          @ [Push <| ty e]
      | _ -> panic ()

  | Stmt.Pop i ->
      match ty i with
      | List t -> simplifyExpr i @ [Pop t]
      | _ -> panic ()

  | Stmt.Write es ->
      let writeSingle e = simplifyExpr e @ [Write <| ty e]
      List.collect writeSingle es @ [WriteLine]

  | Stmt.If (c, t, e) -> simplifyExpr c @ [If (simplifyStmt t, simplifyStmt e)]
  | Stmt.While (c, s) -> [While (simplifyExpr c, simplifyStmt s)]
  | Stmt.DoWhile (s, c) -> [DoWhile (simplifyStmt s, simplifyExpr c)]

  | Stmt.For (i, a, down, b, s) ->
      let comp, arith =
        if down then
          Gte, Sub
        else
          Lte, Add

      let cond = [LoadVar i] @ simplifyExpr b @ [Comp (comp, false)]
      let update = [LoadVar i; PushInt 1; Arith arith; SetVar i]
      simplifyExpr a @ [SetVar i; For (cond, simplifyStmt s, update)]

  | Stmt.Break -> [Break]
  | Stmt.Continue -> [Continue]

  | Seq (a, b) -> simplifyStmt a @ simplifyStmt b
  | Nop -> []

let simplify (vars, stmt) = vars, simplifyStmt stmt
