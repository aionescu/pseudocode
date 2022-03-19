module Compiler.TypeCheck

open Utils.Misc
open Utils.TC
open Compiler.AST

type Env =
  { fns: Map<Id, FnSig>
    vars: Map<Id, Ty>
    inLoop: bool
    retTy: Ty option
  }

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

let mustBe expected actual =
  unless (actual = expected) <|
    err $"Expected type {showTy expected}, but found {showTy actual}"

let rec mustBeLValue (U expr) =
  match expr with
  | Subscript (a, _) -> mustBeLValue a
  | Var _ -> pure' ()
  | _ -> err "Expected lvalue"

let rec typeCheckArgs { name = name; args = fArgs } args =
  let types = List.map snd fArgs
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

and typeCheckExpr t (U expr) =
  match expr with
  | BoolLit b -> mustBe t Bool &> T (Bool, BoolLit b)
  | IntLit i -> mustBe t Int &> T (Int, IntLit i)
  | FloatLit f -> mustBe t Float &> T (Float, FloatLit f)
  | StringLit s -> mustBe t String &> T (String, StringLit s)

  | ListLit es ->
      match t with
      | List t ->
          traverse (typeCheckExpr t) es >>= fun ts ->
          if List.forall (fun e -> ty e = t) ts then
            pure' (T (List t, ListLit ts))
          else
            err "Mismatched types in list literal"
      | _ -> err $"Expected type {showTy t}, but found list literal"

  | Var i -> lookupVar i >>= fun t' -> mustBe t t' &> T (t', Var i)

  | Read e ->
      typeCheckExpr String e >>= fun e ->
      mustNotBeList "read" t &> T (t, Read e)

  | Length e ->
      mustBe t Int *>
      typeInferExpr e >>= fun e ->

      match ty e with
      | String | List _ -> pure' <| T (Int, Length e)
      | _ -> err "Can only take the length of lists and strings"

  | Subscript (a, i) ->
      typeCheckExpr (List t) a >>= fun a ->
      typeCheckExpr Int i <&> fun i ->
      T (t, Subscript (a, i))

  | Not e ->
      mustBe t Bool *>
      typeCheckExpr Bool e <&> fun e ->
      T (Bool, Not e)

  | Negate e ->
      mustBeNumeric t *>
      typeCheckExpr t e <&> fun e ->
      T (t, Negate e)

  | Append (a, b) ->
      mustBe t String *>
      typeCheckExpr String a >>= fun a ->
      typeCheckExpr String b <&> fun b ->
      T (String, Append (a, b))

  | Pow (a, b) ->
      mustBe t Float *>
      typeCheckExpr Float a >>= fun a ->
      typeCheckExpr Float b <&> fun b ->
      T (t, Pow (a, b))

  | Arith (op, a, b) ->
      mustBeNumeric t *>
      typeCheckExpr t a >>= fun a ->
      typeCheckExpr t b <&> fun b ->
      T (t, Arith (op, a, b))

  | Comp (op, a, b) ->
      mustBe t Bool *>
      typeInferExpr a >>= fun a ->
      mustNotBeList "compared" (ty a) *>
      typeCheckExpr (ty a) b <&> fun b ->
      T (Bool, Comp (op, a, b))

  | Logic (op, a, b) ->
      mustBe t Bool *>
      typeCheckExpr Bool a >>= fun a ->
      typeCheckExpr Bool b <&> fun b ->
      T (Bool, Logic (op, a, b))

  | FnCall (f, args) ->
      typeCheckFnCall f args >>= fun (retTy, args) ->
      match retTy with
      | Some t' -> mustBe t t' &> T (t, FnCall (f, args))
      | None -> err "Cannot use functions with no return type as expressions"

and typeInferExpr (U expr) =
  match expr with
  | BoolLit b -> pure' <| T (Bool, BoolLit b)
  | IntLit i -> pure' <| T (Int, IntLit i)
  | FloatLit f -> pure' <| T (Float, FloatLit f)
  | StringLit s -> pure' <| T (String, StringLit s)

  | ListLit [] -> err "Cannot infer the type of empty list literals; Please add a type annotation"
  | ListLit es ->
      traverse typeInferExpr es >>= function
        | (T (t, _) :: ts) as es when List.forall (fun e -> ty e = t) ts ->
            pure' <| T (List t, ListLit es)
        | _ -> err "Mismatched types in list literal"

  | Var i -> lookupVar i <&> fun t -> T (t, Var i)

  | Read _ -> err "Can't infer the type of read-expressions; Please add a type annotation"

  | Length e ->
      typeInferExpr e >>= fun e ->

      match ty e with
      | String | List _ -> pure' <| T (Int, Length e)
      | _ -> err "Can only take the length of lists and strings"

  | Subscript (a, i) ->
      typeInferExpr a >>= fun a ->
      match ty a with
      | List t ->
          typeCheckExpr Int i <&> fun i ->
          T (t, Subscript (a, i))
      | _ -> err "Cannot subscript into non-list values"

  | Not e ->
      typeCheckExpr Bool e <&> fun e ->
      T (Bool, Not e)

  | Negate e ->
      typeInferExpr e >>= fun e ->
      mustBeNumeric (ty e) &>
      T (ty e, Negate e)

  | Append (a, b) ->
      typeCheckExpr String a >>= fun a ->
      typeCheckExpr String b <&> fun b ->
      T (String, Append (a, b))

  | Pow (a, b) ->
      typeCheckExpr Float a >>= fun a ->
      typeCheckExpr Float b <&> fun b ->
      T (Float, Pow (a, b))

  | Arith (op, a, b) ->
      typeInferExpr a >>= fun a ->
      let t = ty a
      mustBeNumeric t *>
      typeCheckExpr t b <&> fun b ->
      T (t, Arith (op, a, b))

  | Comp (op, a, b) ->
      typeInferExpr a >>= fun a ->
      let t = ty a
      mustNotBeList "compared" t *>
      typeCheckExpr t b <&> fun b ->
      T (Bool, Comp (op, a, b))

  | Logic (op, a, b) ->
      typeCheckExpr Bool a >>= fun a ->
      typeCheckExpr Bool b <&> fun b ->
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
      let tcExpr = option typeInferExpr typeCheckExpr t

      tcExpr e >>= fun e ->
      let t = ty e

      local (fun e -> { e with vars = Map.add i t e.vars }) (typeCheckStmt s) >>= fun s ->
      pure' <| Let (i, Some t, e, s)

  | Assign (lhs, e) ->
      mustBeLValue lhs *>
      typeInferExpr lhs >>= fun lhs ->
      let t = ty lhs
      typeCheckExpr t e <&> fun e ->
      Assign (lhs, e)

  | Push (lhs, es) ->
      mustBeLValue lhs *>
      typeInferExpr lhs >>= fun lhs ->
      match ty lhs with
      | List t ->
          traverse (typeCheckExpr t) es <&> fun es ->
          Push (lhs, es)
      | _ -> err "Can only push onto lists"

  | Pop e ->
      typeInferExpr e >>= fun e ->
      match ty e with
      | List t -> pure' (Pop e)
      | _ -> err "Can only pop from lists"

  | Write es ->
      traverse typeInferExpr es >>= fun es ->
      traverse_ (fun (T (t, _)) -> mustNotBeList "written" t) es &>
      Write es

  | If (c, then', else') ->
      typeCheckExpr Bool c >>= fun c ->
      typeCheckStmt then' >>= fun then' ->
      typeCheckStmt else' <&> fun else' ->
      If (c, then', else')

  | While (c, s) ->
      typeCheckExpr Bool c >>= fun c ->
      local (fun e -> { e with inLoop = true }) (typeCheckStmt s) <&> fun s ->
      While (c, s)

  | DoWhile (s, c) ->
      local (fun e -> { e with inLoop = true }) (typeCheckStmt s) >>= fun s ->
      typeCheckExpr Bool c <&> fun c ->
      DoWhile (s, c)

  | For (i, a, down, b, s) ->
      mustBeUndeclared i *>
      typeCheckExpr Int a >>= fun a ->
      typeCheckExpr Int b >>= fun b ->
      local
        (fun e -> { e with vars = Map.add i Int e.vars; inLoop = true })
        (typeCheckStmt s)
      <&> fun s -> For (i, a, down, b, s)

  | Break -> mustBeInLoop "break" &> Break
  | Continue -> mustBeInLoop "continue" &> Continue

  | Return e ->
      asks (fun e -> e.retTy) >>= fun ret ->

      match e, ret with
      | Some e, Some t -> typeCheckExpr t e <&> (Some >> Return)
      | None, None -> pure' <| Return None
      | Some e, None -> err "Cannot return a value from a function with no return type"
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
  | name -> $"function \"name\""

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