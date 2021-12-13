module Midend.Renamer

open Utils.Result
open Frontend.Syntax
open Frontend.TypeChecker
open Utils.Function

type TyIdx = Type * int

let panic () = failwith "Panic in Renamer"

let unionWith f a b =
  let add a k v =
    a |> Map.change k (function
      | None -> Some v
      | Some v' -> Some (f v' v)
    )

  Map.fold add a b

let rec renameExpr env = mapEx <| fun expr ->
    match expr with
    | BoolLit b -> BoolLit b
    | IntLit i -> IntLit i
    | RealLit r -> RealLit r
    | TextLit t -> TextLit t
    | ArrayLit es -> ArrayLit <| List.map (renameExpr env) es
    | Var i -> Var <| Map.find i env
    | Subscript (a, i) -> Subscript (renameExpr env a, renameExpr env i)
    | Not e -> Not <| renameExpr env e
    | Negate e -> Negate <| renameExpr env e
    | Append (a, b) -> Append (renameExpr env a, renameExpr env b)
    | Pow (a, b) -> Pow (renameExpr env a, renameExpr env b)
    | Arith (op, a, b) -> Arith (op, renameExpr env a, renameExpr env b)
    | Comp (op, a, b) -> Comp (op, renameExpr env a, renameExpr env b)
    | Logic (op, a, b) -> Logic (op, renameExpr env a, renameExpr env b)

let allocVar env live max' ty name =
  let idx =
    Map.tryFind ty live
    |> Option.map ((+) 1)
    |> flip defaultArg 0

  let live = Map.add ty idx live
  (Map.add name (ty, idx) env, live, unionWith max max' live, idx)

let rec renameStmt env live max stmt =
  match stmt with
  | Let (_, None, _) -> panic ()
  | Let (i, Some t, e) ->
      let env', live, max, idx = allocVar env live max t i
      env', live, max, Let ((t, idx), Some t, renameExpr env e)

  | Assign (i, e) -> env, live, max, Assign (renameExpr env i, renameExpr env e)
  | Read e -> env, live, max, Read (renameExpr env e)
  | Write es -> env, live, max, Write <| List.map (renameExpr env) es

  | If (c, t, e) ->
      let _, _, max, t = renameStmts env live max t
      let _, _, max, e = renameStmts env live max e

      env, live, max, If (renameExpr env c, t, e)

  | While (c, s) ->
      let _, _, max, s = renameStmts env live max s
      env, live, max, While (renameExpr env c, s)

  | For (i, a, b, s) ->
      let env', live', max, idx = allocVar env live max Int i
      let _, _, max, s = renameStmts env' live' max s
      env, live, max, For ((Int, idx), renameExpr env a, renameExpr env b, s)

and renameStmts env live max = function
  | [] -> env, live, max, []
  | stmt :: stmts ->
      let env, live, max, stmt = renameStmt env live max stmt
      let env, live, max, stmts = renameStmts env live max stmts
      env, live, max, stmt :: stmts

let renameProgram stmts : _ * Stmt<TyIdx, TExpr<TyIdx>> list =
  let _, _, max, stmts = renameStmts Map.empty Map.empty Map.empty stmts
  max, stmts
