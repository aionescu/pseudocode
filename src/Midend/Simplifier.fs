module Midend.Simplifier

open Utils.Function
open Frontend.Syntax
open Midend.Core

let panic () = failwith "Panic in Simplifier"

let allocVars maxLive =
  Map.toList maxLive
  |> List.collect (fun (ty, max) -> List.map (fun i -> ty, i) [0 .. max])
  |> List.indexed
  |> List.map swap
  |> Map.ofList

let varList maxLive =
  Map.toList maxLive
  |> List.collect (fun (ty, max) -> List.replicate (max + 1) ty)

let rec simplifyExpr env (T (t, e)) =
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

      let setElem i e = [Dup; PushInt i] @ simplifyExpr env e @ [SetIndex]
      let l = List.length es
      [PushInt l; NewArr t] @ List.collect (uncurry setElem) (List.indexed es)

  | Var i -> [LoadVar <| Map.find i env]

  | Expr.Read None -> [Read t]
  | Expr.Read (Some e) -> simplifyExpr env e @ [Write Text; Read t]

  | Subscript (a, i) -> simplifyExpr env a @ simplifyExpr env i @ [LoadIndex]
  | Expr.Not e -> simplifyExpr env e @ [Not]
  | Expr.Negate e -> simplifyExpr env e @ [Negate]

  | Expr.Append (a, b) -> simplifyExpr env a @ simplifyExpr env b @ [Append (t <> Text)]
  | Expr.Pow (a, b) -> simplifyExpr env a @ simplifyExpr env b @ [Pow]
  | Expr.Arith (op, a, b) -> simplifyExpr env a @ simplifyExpr env b @ [Arith op]
  | Expr.Comp (op, a, b) -> simplifyExpr env a @ simplifyExpr env b @ [Comp (op, t = Text)]

  | Logic (And, a, b) -> simplifyExpr env a @ [If (simplifyExpr env b, [PushBool false])]
  | Logic (Or, a, b) -> simplifyExpr env a @ [If ([PushBool true], simplifyExpr env b)]

let rec simplifyStmt env stmt =
  match stmt with
  | Let (i, _, e)
  | Assign (T (_, Var i), e) -> simplifyExpr env e @ [SetVar <| Map.find i env]

  | Assign (T (_, Subscript (a, i)), e) ->
      simplifyExpr env a @ simplifyExpr env i @ simplifyExpr env e @ [SetIndex]
  | Assign _ -> panic ()

  | Stmt.Write es ->
      let writeSingle (T (t, _) as e) = simplifyExpr env e @ [Write t]
      List.collect writeSingle es @ [WriteLine]

  | Stmt.If (c, t, e) -> simplifyExpr env c @ [If (simplifyStmts env t, simplifyStmts env e)]
  | Stmt.While (c, s) -> [While (simplifyExpr env c, simplifyStmts env s)]

  | For (i, a, b, s) ->
      let idx = Map.find i env
      let cond = [LoadVar idx] @ simplifyExpr env b @ [Comp (Lte, false)]
      let body = simplifyStmts env s @ [LoadVar idx; PushInt 1; Arith Add; SetVar idx]
      simplifyExpr env a @ [SetVar idx; While (cond, body)]

and simplifyStmts env stmts = List.collect (simplifyStmt env) stmts

let simplifyProgram max stmts = simplifyStmts (allocVars max) stmts