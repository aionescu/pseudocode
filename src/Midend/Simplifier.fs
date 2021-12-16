module Midend.Simplifier

open Utils.Function
open Frontend.Syntax
open Midend.Core

let panic () = failwith "Panic in Simplifier"

let rec simplifyExpr (T (t, e)) =
  match e with
  | BoolLit b -> [PushBool b]
  | IntLit i -> [PushInt i]
  | RealLit r -> [PushReal r]
  | TextLit t -> [PushText t]

  | ArrayLit es ->
      let t =
        match t with
        | Array t -> t
        | _ -> panic ()

      let setElem i e = [Dup; PushInt i] @ simplifyExpr e @ [SetIndex t]
      let l = List.length es
      [PushInt l; NewArr t] @ List.collect (uncurry setElem) (List.indexed es)

  | Var i -> [LoadVar i]

  | Expr.Read e -> simplifyExpr e @ [Write Text; Read t]
  | Expr.Length e -> simplifyExpr e @ [Length (t = Text)]

  | Subscript (a, i) -> simplifyExpr a @ simplifyExpr i @ [LoadIndex t]
  | Expr.Not e -> simplifyExpr e @ [Not]
  | Expr.Negate e -> simplifyExpr e @ [Negate]

  | Expr.Append (a, b) -> simplifyExpr a @ simplifyExpr b @ [Append]
  | Expr.Pow (a, b) -> simplifyExpr a @ simplifyExpr b @ [Pow]
  | Expr.Arith (op, a, b) -> simplifyExpr a @ simplifyExpr b @ [Arith op]
  | Expr.Comp (op, a, b) -> simplifyExpr a @ simplifyExpr b @ [Comp (op, t = Text)]

  | Logic (And, a, b) -> simplifyExpr a @ [If (simplifyExpr b, [PushBool false])]
  | Logic (Or, a, b) -> simplifyExpr a @ [If ([PushBool true], simplifyExpr b)]

let rec simplifyStmt stmt =
  match stmt with
  | Let (i, _, e)

  | Assign (T (_, Var i), e) -> simplifyExpr e @ [SetVar i]
  | Assign (T (t, Subscript (a, i)), e) ->
      simplifyExpr a @ simplifyExpr i @ simplifyExpr e @ [SetIndex t]
  | Assign _ -> panic ()

  | Stmt.Write es ->
      let writeSingle (T (t, _) as e) = simplifyExpr e @ [Write t]
      List.collect writeSingle es @ [WriteLine]

  | Stmt.If (c, t, e) -> simplifyExpr c @ [If (simplifyStmts t, simplifyStmts e)]
  | Stmt.While (c, s) -> [While (simplifyExpr c, simplifyStmts s)]

  | For (i, a, b, s) ->
      let cond = [LoadVar i] @ simplifyExpr b @ [Comp (Lte, false)]
      let body = simplifyStmts s @ [LoadVar i; PushInt 1; Arith Add; SetVar i]
      simplifyExpr a @ [SetVar i; While (cond, body)]

and simplifyStmts stmts = List.collect simplifyStmt stmts

let simplifyProgram = simplifyStmts
