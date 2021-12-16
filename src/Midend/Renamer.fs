module Midend.Renamer

open Utils.State
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
    | Read e -> Read <| Option.map (renameExpr env) e
    | Subscript (a, i) -> Subscript (renameExpr env a, renameExpr env i)
    | Not e -> Not <| renameExpr env e
    | Negate e -> Negate <| renameExpr env e
    | Append (a, b) -> Append (renameExpr env a, renameExpr env b)
    | Pow (a, b) -> Pow (renameExpr env a, renameExpr env b)
    | Arith (op, a, b) -> Arith (op, renameExpr env a, renameExpr env b)
    | Comp (op, a, b) -> Comp (op, renameExpr env a, renameExpr env b)
    | Logic (op, a, b) -> Logic (op, renameExpr env a, renameExpr env b)

type VarState = {
  env: Map<Id, TyIdx>
  live: Map<Type, int>
  max: Map<Type, int>
}

let allocVar ty name =
  get >>= fun { env = env; live = live; max = max' } ->

  let idx =
    Map.tryFind ty live
    |> Option.map ((+) 1)
    |> flip defaultArg 0

  let live = Map.add ty idx live

  idx <& put {
    env = Map.add name (ty, idx) env
    live = live
    max = unionWith max max' live
  }

let scoped m =
  get >>= fun old ->
  m >>= fun a ->
  get >>= fun { max = max } ->
  put { old with max = max } &> a

let rec renameStmt stmt =
  get >>= fun { env = env } ->
  match stmt with
  | Let (_, None, _) -> panic ()
  | Let (i, Some t, e) ->
      allocVar t i <&> fun i ->
      Let ((t, i), Some t, renameExpr env e)

  | Assign (i, e) -> pure' <| Assign (renameExpr env i, renameExpr env e)
  | Write es -> pure' (Write <| List.map (renameExpr env) es)

  | If (c, t, e) ->
      scoped (traverse renameStmt t) >>= fun t ->
      scoped (traverse renameStmt e) <&> fun e ->
      If (renameExpr env c, t, e)

  | While (c, s) ->
      scoped (traverse renameStmt s) <&> fun s ->
      While (renameExpr env c, s)

  | For (i, a, b, s) ->
      scoped (pair <!> allocVar Int i <*> traverse renameStmt s) <&> fun (i, s) ->
      For ((Int, i), renameExpr env a, renameExpr env b, s)

let renameProgram stmts : _ * Stmt<TyIdx, TExpr<TyIdx>> list =
  traverse renameStmt stmts
  |> runState { env = Map.empty; live = Map.empty; max = Map.empty }
  |> second (fun v -> v.max)
  |> swap
