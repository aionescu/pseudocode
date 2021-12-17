module Frontend.TypeChecker

open Utils.Misc
open Utils.Monad.Result
open Frontend.Syntax

let mustBeNumeric = function
  | Int | Float -> Ok ()
  | t -> Error $"Expected numeric type, found {showType t}"

let mustNotBeList reason = function
  | List _ -> Error $"Values of list types cannot be {reason}"
  | _ -> Ok ()

let lookupVar i env =
  Map.tryFind i env
  |> explain $"Undeclared variable \"{i}\""

let mustBe expected actual =
  if actual = expected then
    Ok ()
  else
    Error $"Expected type {showType expected}, but found {showType actual}"

let rec mustBeLValue (U expr) =
  match expr with
  | Subscript (a, _) -> mustBeLValue a
  | Var _ -> Ok ()
  | _ -> Error "Expected lvalue"

let rec typeCheckExpr env t (U expr) =
  match expr with
  | BoolLit b -> mustBe t Bool &> T (Bool, BoolLit b)
  | IntLit i -> mustBe t Int &> T (Int, IntLit i)
  | FloatLit f -> mustBe t Float &> T (Float, FloatLit f)
  | StringLit s -> mustBe t String &> T (String, StringLit s)

  | ListLit es ->
      match t with
      | List t ->
          traverse (typeCheckExpr env t) es >>= fun ts ->
          if List.forall (fun e -> ty e = t) ts then
            Ok (T (List t, ListLit ts))
          else
            Error "Mismatched types in list literal"
      | _ -> Error $"Expected type {showType t}, but found list literal"

  | Var i -> lookupVar i env >>= fun t' -> mustBe t t' &> T (t', Var i)

  | Read e ->
      typeCheckExpr env String e >>= fun e ->
      mustNotBeList "read" t &> T (t, Read e)

  | Length e ->
      mustBe t Int *>
      typeInferExpr env e >>= fun e ->

      match ty e with
      | String | List _ -> Ok <| T (Int, Length e)
      | _ -> Error "Can only take the length of lists and strings"

  | Subscript (a, i) ->
      typeCheckExpr env (List t) a >>= fun a ->
      typeCheckExpr env Int i <&> fun i ->
      T (t, Subscript (a, i))

  | Not e ->
      mustBe t Bool *>
      typeCheckExpr env Bool e <&> fun e ->
      T (Bool, Not e)

  | Negate e ->
      mustBeNumeric t *>
      typeCheckExpr env t e <&> fun e ->
      T (t, Negate e)

  | Append (a, b) ->
      mustBe t String *>
      typeCheckExpr env String a >>= fun a ->
      typeCheckExpr env String b <&> fun b ->
      T (String, Append (a, b))

  | Pow (a, b) ->
      mustBe t Float *>
      typeCheckExpr env Float a >>= fun a ->
      typeCheckExpr env Float b <&> fun b ->
      T (t, Pow (a, b))

  | Arith (op, a, b) ->
      mustBeNumeric t *>
      typeCheckExpr env t a >>= fun a ->
      typeCheckExpr env t b <&> fun b ->
      T (t, Arith (op, a, b))

  | Comp (op, a, b) ->
      mustBe t Bool *>
      typeInferExpr env a >>= fun a ->
      mustNotBeList "compared" (ty a) *>
      typeCheckExpr env (ty a) b <&> fun b ->
      T (Bool, Comp (op, a, b))

  | Logic (op, a, b) ->
      mustBe t Bool *>
      typeCheckExpr env Bool a >>= fun a ->
      typeCheckExpr env Bool b <&> fun b ->
      T (Bool, Logic (op, a, b))

and typeInferExpr env (U expr) =
  match expr with
  | BoolLit b -> Ok <| T (Bool, BoolLit b)
  | IntLit i -> Ok <| T (Int, IntLit i)
  | FloatLit f -> Ok <| T (Float, FloatLit f)
  | StringLit s -> Ok <| T (String, StringLit s)

  | ListLit [] -> Error "Cannot infer the type of empty list literals; Please add a type annotation"
  | ListLit es ->
      traverse (typeInferExpr env) es >>= function
        | (T (t, _) :: ts) as es when List.forall (fun e -> ty e = t) ts ->
            Ok <| T (List t, ListLit es)
        | _ -> Error "Mismatched types in list literal"

  | Var i -> lookupVar i env <&> fun t -> T (t, Var i)

  | Read _ -> Error "Can't infer the type of read-expressions; Please add a type annotation"

  | Length e ->
      typeInferExpr env e >>= fun e ->

      match ty e with
      | String | List _ -> Ok <| T (Int, Length e)
      | _ -> Error "Can only take the length of lists and strings"

  | Subscript (a, i) ->
      typeInferExpr env a >>= fun a ->
      match ty a with
      | List t ->
          typeCheckExpr env Int i <&> fun i ->
          T (t, Subscript (a, i))
      | _ -> Error "Cannot subscript into non-list values"

  | Not e ->
      typeCheckExpr env Bool e <&> fun e ->
      T (Bool, Not e)

  | Negate e ->
      typeInferExpr env e >>= fun e ->
      mustBeNumeric (ty e) &>
      T (ty e, Negate e)

  | Append (a, b) ->
      typeCheckExpr env String a >>= fun a ->
      typeCheckExpr env String b <&> fun b ->
      T (String, Append (a, b))

  | Pow (a, b) ->
      typeCheckExpr env Float a >>= fun a ->
      typeCheckExpr env Float b <&> fun b ->
      T (Float, Pow (a, b))

  | Arith (op, a, b) ->
      typeInferExpr env a >>= fun a ->
      let t = ty a
      mustBeNumeric t *>
      typeCheckExpr env t b <&> fun b ->
      T (t, Arith (op, a, b))

  | Comp (op, a, b) ->
      typeInferExpr env a >>= fun a ->
      let t = ty a
      mustNotBeList "compared" t *>
      typeCheckExpr env t b <&> fun b ->
      T (Bool, Comp (op, a, b))

  | Logic (op, a, b) ->
      typeCheckExpr env Bool a >>= fun a ->
      typeCheckExpr env Bool b <&> fun b ->
      T (Bool, Logic (op, a, b))

let rec typeCheckStmt env inLoop stmt =
  match stmt with
  | Let (i, _, _) when Map.containsKey i env -> Error $"Variable \"{i}\" already declared"
  | Let (i, Some t, e) ->
      typeCheckExpr env t e <&> fun e ->
      (Map.add i t env, Let (i, Some t, e))
  | Let (i, None, e) ->
      typeInferExpr env e <&> fun e ->
      let t = ty e
      (Map.add i t env, Let (i, Some t, e))

  | Assign (lhs, e) ->
      mustBeLValue lhs *>
      typeInferExpr env lhs >>= fun lhs ->
      let t = ty lhs
      typeCheckExpr env t e <&> fun e ->
      (env, Assign (lhs, e))

  | Push (lhs, es) ->
      mustBeLValue lhs *>
      typeInferExpr env lhs >>= fun lhs ->
      match ty lhs with
      | List t ->
          traverse (typeCheckExpr env t) es <&> fun es ->
          (env, Push (lhs, es))
      | _ -> Error "Can only push onto lists"

  | Pop e ->
      typeInferExpr env e >>= fun e ->
      match ty e with
      | List t -> Ok (env, Pop e)
      | _ -> Error "Can only pop from lists"

  | Write es ->
      traverse (typeInferExpr env) es >>= fun es ->
      traverse_ (fun (T (t, _)) -> mustNotBeList "written" t) es &>
      (env, Write es)

  | If (c, then', else') ->
      typeCheckExpr env Bool c >>= fun c ->
      typeCheckStmts env inLoop then' >>= fun (_, then') ->
      typeCheckStmts env inLoop else' >>= fun (_, else') ->
      Ok (env, If (c, then', else'))

  | While (c, s) ->
      typeCheckExpr env Bool c >>= fun c ->
      typeCheckStmts env true s <&> fun (_, s) ->
      (env, While (c, s))

  | DoWhile (s, c) ->
      typeCheckStmts env true s >>= fun (env', s) ->
      typeCheckExpr env' Bool c <&> fun c ->
      (env, DoWhile (s, c))

  | For (i, _, _, _, _) when Map.containsKey i env -> Error "For loop counter must be a new variable"
  | For (i, a, down, b, s) ->
      typeCheckExpr env Int a >>= fun a ->
      typeCheckExpr env Int b >>= fun b ->
      typeCheckStmts (Map.add i Int env) true s <&> fun (_, s) ->
      (env, For (i, a, down, b, s))

  | Break when inLoop -> Ok (env, Break)
  | Break -> Error "Break can only be used inside a loop."

  | Continue when inLoop -> Ok (env, Continue)
  | Continue -> Error "Continue can only be used inside a loop."

and typeCheckStmts env inLoop = function
  | [] -> Ok (env, [])
  | stmt :: stmts ->
      typeCheckStmt env inLoop stmt >>= fun (env, stmt) ->
      typeCheckStmts env inLoop stmts <&> fun (env, stmts) -> (env, stmt :: stmts)

let typeCheckProgram stmts = typeCheckStmts Map.empty false stmts <&> snd
