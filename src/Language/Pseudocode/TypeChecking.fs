module Language.Pseudocode.TypeChecking

open Utils.Misc
open Control.Monad.TC
open Language.Pseudocode.Syntax

type Env =
  { fns: Map<Id, FnSig>
    vars: Map<Id, Ty>
    inLoop: bool
    retTy: Ty option
  }

let explain e = function
  | None -> Error e
  | Some a -> Ok a

let mustBeNumeric = function
  | Int | Float -> pure' ()
  | t -> err $"Expected numeric type, found {showTy t}"

let mustNotBeList reason = function
  | List _ -> err $"Values of list types cannot be {reason}"
  | _ -> pure' ()

let lookupVar v =
  asks (fun e -> Map.tryFind v e.vars)
  >>= (explain $"Undeclared variable \"{v}\"" >> lift)

let lookupFn f =
  asks (fun e -> Map.tryFind f e.fns)
  >>= (explain $"Undeclared function \"{f}\"" >> lift)

let mustBeHint hint e =
  match hint with
  | Some t when t <> ty e -> err $"Expected type {showTy t}, but found {showTy <| ty e}"
  | _ -> pure' e

let rec mustBeLValue (U expr) =
  match expr with
  | Subscript (a, _) -> mustBeLValue a
  | Var _ -> pure' ()
  | _ -> err "Expected lvalue"

let rec typeCheckArgs { name = name; args = fArgs } args =
  let types = List.map (Some << snd) fArgs
  let ordering = compare (List.length args) (List.length types)

  if ordering <> 0 then
    err $"""Function "{name}" called with too {if ordering < 0 then "few" else "many"} arguments"""
  else
    List.zip types args |> traverse (uncurry typeCheckExpr)

and typeCheckFnCall f args =
  ask >>= fun { vars = vars; fns = fns } ->
  lookupFn f >>= fun f ->
  typeCheckArgs f args <&> fun args ->
  f.retTy, args

and typeCheckExpr hint (U expr) =
  mustBeHint hint =<<
  match expr with
  | BoolLit b -> pure' <| T (Bool, BoolLit b)
  | IntLit i -> pure' <| T (Int, IntLit i)
  | FloatLit f -> pure' <| T (Float, FloatLit f)
  | StringLit s -> pure' <| T (String, StringLit s)

  | ListLit es ->
      let mustAllBe t es =
        when'
          (not <| List.forall (fun e -> ty e = t) es)
          (err "Mismatched types in list literal")
        &> T (List t, ListLit es)

      match es, hint with
      | _, Some (List t) -> traverse (typeCheckExpr <| Some t) es <&> fun es -> T (List t, ListLit es)
      | e :: es, None ->
          typeCheckExpr None e >>= fun (T (t, _) as e) ->
          traverse (typeCheckExpr <| Some t) es >>= fun es ->
          mustAllBe t (e :: es)
      | [], None -> err "Cannot infer the type of empty list literals; Please add a type annotation"
      | _, Some t -> err $"Expected {showTy t}, but found list literal"

  | Var i -> lookupVar i <&> fun t -> T (t, Var i)

  | Read e ->
      match hint with
      | None -> err "Can't infer the type of read-expressions; Please add a type annotation"
      | Some t ->
          typeCheckExpr (Some String) e >>= fun e ->
          mustNotBeList "read" t &> T (t, Read e)

  | Length e ->
      typeCheckExpr None e >>= fun e ->

      match ty e with
      | String | List _ -> pure' <| T (Int, Length e)
      | _ -> err "Can only take the length of lists and strings"

  | Subscript (a, i) ->
      typeCheckExpr (Option.map List hint) a >>= fun a ->
      match ty a with
      | List t ->
          typeCheckExpr (Some Int) i <&> fun i ->
          T (t, Subscript (a, i))
      | _ -> err "Cannot subscript into non-list values"

  | Not e ->
      typeCheckExpr (Some Bool) e <&> fun e ->
      T (Bool, Not e)

  | Negate e ->
      typeCheckExpr None e >>= fun e ->
      mustBeNumeric (ty e) &>
      T (ty e, Negate e)

  | Append (a, b) ->
      typeCheckExpr (Some String) a >>= fun a ->
      typeCheckExpr (Some String) b <&> fun b ->
      T (String, Append (a, b))

  | Pow (a, b) ->
      typeCheckExpr (Some Float) a >>= fun a ->
      typeCheckExpr (Some Float) b <&> fun b ->
      T (Float, Pow (a, b))

  | Arith (op, a, b) ->
      typeCheckExpr None a >>= fun a ->
      let t = ty a
      mustBeNumeric t *>
      typeCheckExpr (Some t) b <&> fun b ->
      T (t, Arith (op, a, b))

  | Comp (op, a, b) ->
      typeCheckExpr None a >>= fun a ->
      let t = ty a
      mustNotBeList "compared" t *>
      typeCheckExpr (Some t) b <&> fun b ->
      T (Bool, Comp (op, a, b))

  | Logic (op, a, b) ->
      typeCheckExpr (Some Bool) a >>= fun a ->
      typeCheckExpr (Some Bool) b <&> fun b ->
      T (Bool, Logic (op, a, b))

  | FnCall (f, args) ->
      typeCheckFnCall f args >>= fun (retTy, args) ->
      match retTy with
      | Some t -> pure' <| T (t, FnCall (f, args))
      | None -> err "Cannot use functions with no return type as expressions"

let mustBeUndeclared i =
  asks (fun e -> e.vars) >>= fun vars ->
  when' (Map.containsKey i vars) <|
    err $"Variable \"{i}\" is already declared"

let mustBeInLoop lbl =
  asks (fun e -> e.inLoop) >>= flip unless (err $"\"{lbl}\" can only be used inside a loop.")

let rec typeCheckStmt stmt =
  match stmt with
  | Let (i, t, e, s) ->
      mustBeUndeclared i *>
      typeCheckExpr t e >>= fun e ->
      let t = ty e

      local (fun e -> { e with vars = Map.add i t e.vars }) (typeCheckStmt s) >>= fun s ->
      pure' <| Let (i, Some t, e, s)

  | Assign (lhs, e) ->
      mustBeLValue lhs *>
      typeCheckExpr None lhs >>= fun lhs ->
      let t = ty lhs
      typeCheckExpr (Some t) e <&> fun e ->
      Assign (lhs, e)

  | Push (lhs, es) ->
      mustBeLValue lhs *>
      typeCheckExpr None lhs >>= fun lhs ->
      match ty lhs with
      | List t ->
          traverse (typeCheckExpr <| Some t) es <&> fun es ->
          Push (lhs, es)
      | _ -> err "Can only push onto lists"

  | Pop e ->
      typeCheckExpr None e >>= fun e ->
      match ty e with
      | List t -> pure' (Pop e)
      | _ -> err "Can only pop from lists"

  | Write es ->
      traverse (typeCheckExpr None) es >>= fun es ->
      traverse_ (fun (T (t, _)) -> mustNotBeList "written" t) es &>
      Write es

  | If (c, then', else') ->
      typeCheckExpr (Some Bool) c >>= fun c ->
      typeCheckStmt then' >>= fun then' ->
      typeCheckStmt else' <&> fun else' ->
      If (c, then', else')

  | While (c, s) ->
      typeCheckExpr (Some Bool) c >>= fun c ->
      local (fun e -> { e with inLoop = true }) (typeCheckStmt s) <&> fun s ->
      While (c, s)

  | DoWhile (s, c) ->
      local (fun e -> { e with inLoop = true }) (typeCheckStmt s) >>= fun s ->
      typeCheckExpr (Some Bool) c <&> fun c ->
      DoWhile (s, c)

  | For (i, a, down, b, s) ->
      mustBeUndeclared i *>
      typeCheckExpr (Some Int) a >>= fun a ->
      typeCheckExpr (Some Int) b >>= fun b ->
      local
        (fun e -> { e with vars = Map.add i Int e.vars; inLoop = true })
        (typeCheckStmt s)
      <&> fun s -> For (i, a, down, b, s)

  | Break -> mustBeInLoop "break" &> Break
  | Continue -> mustBeInLoop "continue" &> Continue

  | Return e ->
      asks (fun e -> e.retTy) >>= fun ret ->

      match e, ret with
      | Some e, Some _ -> typeCheckExpr ret e <&> (Some >> Return)
      | None, None -> pure' <| Return None
      | Some _, None -> err "Cannot return a value from a function with no return type"
      | None, Some t -> err "Cannot use return with no value in a function with a return type"

  | FnCallStmt (f, args) ->
      typeCheckFnCall f args >>= fun (retTy, args) ->
      match retTy with
      | None -> pure' <| FnCallStmt (f, args)
      | Some _ -> err "Cannot use functions with return type as statements"

  | Seq (a, b) -> curry Seq <!> typeCheckStmt a <*> typeCheckStmt b
  | Nop -> pure' Nop

let checkDuplicateArgs args =
  match duplicatesBy fst args with
  | [] -> pure' ()
  | arg :: _ -> err $"Duplicate definition for argument \"{arg}\""

let rec alwaysReturns = function
  | Return _ -> true
  | Let (_, _, _, s) -> alwaysReturns s
  | Seq (a, b) -> alwaysReturns a || alwaysReturns b
  | If (_, t, f) -> alwaysReturns t && alwaysReturns f
  | DoWhile (s, _) -> alwaysReturns s
  | _ -> false

let flowAnalysis retTy body =
  unless (alwaysReturns body || retTy = None) <|
    err $"Not all code paths return a value"

let fnName = function
  | "program" -> "program"
  | name -> $"function \"{name}\""

let typeCheckFn { name = name; args = args; retTy = retTy } body =
  checkDuplicateArgs args
  *> local
    (fun e -> { e with vars = Map.ofList args; retTy = retTy })
    (typeCheckStmt body)
  <* flowAnalysis retTy body
  |> mapErr ((+) $"In {fnName name}: ")

let typeCheck p =
  traverseFns typeCheckFn p
  |> mapErr ((+) "Type error: ")
  |> runTC
    { fns = mapVals fst p.fns
      vars = Map.empty
      inLoop = false
      retTy = None
    }
