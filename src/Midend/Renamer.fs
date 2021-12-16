module Midend.Renamer

open Utils.Function
open Utils.State
open Frontend.Syntax

type Idx = Core.Idx

let panic () = failwith "Panic in Renamer"

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

type RenamerState = {
  env: Map<Id, Idx>
  vars: Map<Idx, Type * bool>
  count: int
  crrScope: (Id * Idx) list
}

let findLive ty = Map.tryFindKey (fun _ (t, live) -> ty = t && not live)

let allocVar ty name =
  get >>= fun { env = env; vars = vars; count = count; crrScope = crrScope } ->

  let env, vars, count, i =
    match findLive ty vars with
    | Some i -> Map.add name i env, Map.add i (ty, true) vars, count, i
    | None -> Map.add name count env, Map.add count (ty, true) vars, count + 1, count

  put { env = env; vars = vars; count = count; crrScope = (name, i) :: crrScope }
  &> i

let deallocVar name i =
  get >>= fun { env = env; vars = vars } ->

  let env = Map.remove name env

  let vars = vars |> Map.change i (function
    | None -> None
    | Some (ty, _) -> Some (ty, false)
  )

  modify (fun s -> { s with env = env; vars = vars })

let scoped m =
  get >>= fun { crrScope = oldScope } ->
  modify (fun s -> { s with crrScope = [] }) *>
  m >>= fun r ->
  gets (fun s -> s.crrScope) >>= fun alloced ->
  traverse_ (uncurry deallocVar) alloced *>
  modify (fun s -> { s with crrScope = oldScope })
  &> r

let rec renameStmt stmt =
  get >>= fun { env = env } ->
  match stmt with
  | Let (_, None, _) -> panic ()
  | Let (i, Some t, e) ->
      allocVar t i <&> fun i ->
      Let (i, Some t, renameExpr env e)

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
      For (i, renameExpr env a, renameExpr env b, s)

let renameProgram stmts =
  traverse renameStmt stmts
  |> runState { env = Map.empty; vars = Map.empty; count = 0; crrScope = [] }
  |> second (fun v -> List.init v.count (flip Map.find v.vars >> fst))
  |> swap
