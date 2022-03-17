module Compiler.Midend.Renamer

#nowarn "40"

open Utils
open Monad.State
open Compiler.Frontend.AST

type Idx = IR.Idx

let panic () = failwith "Panic in Renamer"

let rec renameExpr (vars: Map<_, _>) args =
  let rec go = mapEx <| fun expr ->
    match expr with
    | BoolLit b -> BoolLit b
    | IntLit i -> IntLit i
    | FloatLit f -> FloatLit f
    | StringLit s -> StringLit s
    | ListLit es -> ListLit <| List.map go es
    | Var i -> Var <| vars[i]
    | Arg i -> Arg <| List.findIndex ((=) i) args
    | Read e -> Read <| go e
    | Length e -> Length <| go e
    | Subscript (a, i) -> Subscript (go a, go i)
    | Not e -> Not <| go e
    | Negate e -> Negate <| go e
    | Append (a, b) -> Append (go a, go b)
    | Pow (a, b) -> Pow (go a, go b)
    | Arith (op, a, b) -> Arith (op, go a, go b)
    | Comp (op, a, b) -> Comp (op, go a, go b)
    | Logic (op, a, b) -> Logic (op, go a, go b)
    | FnCall (f, args) -> FnCall (f, List.map go args)

  go

type RenamerState =
  { vars: Map<Id, Idx>
    live: Map<Idx, Type * bool>
    args: Id list
    count: int
    crrScope: (Id * Idx) list
  }

let findLive ty = Map.tryFindKey (fun _ (t, live) -> ty = t && not live)

let allocVar ty name =
  get () >>= fun { vars = vars; live = live; args = args; count = count; crrScope = crrScope } ->

  let vars, live, count, i =
    match findLive ty live with
    | Some i -> Map.add name i vars, Map.add i (ty, true) live, count, i
    | None -> Map.add name count vars, Map.add count (ty, true) live, count + 1, count

  put { vars = vars; live = live; args = args; count = count; crrScope = (name, i) :: crrScope }
  &> i

let deallocVar name i =
  get () >>= fun { vars = vars; live = live } ->

  let vars = Map.remove name vars

  let live = live |> Map.change i (function
    | None -> None
    | Some (ty, _) -> Some (ty, false)
  )

  modify (fun s -> { s with vars = vars; live = live })

let scoped m =
  get () >>= fun { crrScope = oldScope } ->
  modify (fun s -> { s with crrScope = [] }) *>
  m >>= fun r ->
  gets (fun s -> s.crrScope) >>= fun alloced ->
  traverse_ (uncurry deallocVar) alloced *>
  modify (fun s -> { s with crrScope = oldScope })
  &> r

let rec renameStmt stmt =
  get () >>= fun { vars = vars; args = args } ->
  let renameExpr = renameExpr vars args

  match stmt with
  | Let (i, Some t, e, s) ->
      allocVar t i >>= fun i ->
      renameStmt s <&> fun s ->
      Let (i, Some t, renameExpr e, s)
  | Let _ -> panic ()

  | Assign (i, e) -> pure' <| Assign (renameExpr i, renameExpr e)
  | Push (i, es) -> pure' <| Push (renameExpr i, List.map renameExpr es)
  | Pop i -> pure' <| Pop (renameExpr i)
  | Write es -> pure' (Write <| List.map renameExpr es)

  | If (c, t, e) ->
      scoped (renameStmt t) >>= fun t ->
      scoped (renameStmt e) <&> fun e ->
      If (renameExpr c, t, e)

  | While (c, s) ->
      scoped (renameStmt s) <&> fun s ->
      While (renameExpr c, s)

  | DoWhile (s, c) ->
      scoped (
        renameStmt s >>= fun s ->
        gets (fun s -> s.vars) <&> fun vars ->
        DoWhile (s, renameExpr c)
      )

  | For (i, a, down, b, s) ->
      scoped (pair <!> allocVar Int i <*> renameStmt s) <&> fun (i, s) ->
      For (i, renameExpr a, down, renameExpr b, s)

  | Break -> pure' Break
  | Continue -> pure' Continue

  | Return e -> pure' (Return <| Option.map renameExpr e)
  | FnCallStmt (f, args) -> pure' <| FnCallStmt (f, List.map renameExpr args)

  | Seq (a, b) ->
      renameStmt a >>= fun a ->
      renameStmt b <&> fun b ->
      Seq (a, b)

  | Nop -> pure' Nop

let renameFn ({ args = args }: FnSig) body =
  renameStmt body
  |> runState
    { vars = Map.empty
      live = Map.empty
      args = List.map fst args
      count = 0
      crrScope = []
    }
  |> second (fun v -> List.init v.count (flip Map.find v.live >> fst))
  |> swap

let rename = mapFns renameFn
