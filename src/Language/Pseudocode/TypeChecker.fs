module Language.Pseudocode.TypeChecker

open Utils.Function
open Utils.Monad.Result
open Language.Pseudocode.Syntax

let mustBeNumeric = function
  | Int | Real -> Ok ()
  | t -> Error $"Expected numeric type, found {showType t}"

let mustBeAppendable = function
  | Array _ | Text -> Ok ()
  | t -> Error $"Only values of Text or array type can be appended, but found {showType t}"

let mustNotBeArray reason = function
  | Array _ -> Error $"Values of array types cannot be {reason}"
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
  | RealLit r -> mustBe t Real &> T (Real, RealLit r)
  | TextLit t' -> mustBe t Text &> T (Text, TextLit t')

  | ArrayLit es ->
      match t with
      | Array t ->
          traverse (typeCheckExpr env t) es >>= fun ts ->
          if List.forall (fun e -> ty e = t) ts then
            Ok (T (Array t, ArrayLit ts))
          else
            Error "Mismatched types in array literal"
      | _ -> Error $"Expected type {showType t}, but found array literal"

  | Var i -> lookupVar i env >>= fun t' -> mustBe t t' &> T (t', Var i)

  | Subscript (a, i) ->
      typeCheckExpr env (Array t) a >>= fun a ->
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
      mustBeAppendable t *>
      typeCheckExpr env t a >>= fun a ->
      typeCheckExpr env t b <&> fun b ->
      T (t, Append (a, b))

  | Pow (a, b) ->
      mustBe t Real *>
      typeCheckExpr env Real a >>= fun a ->
      typeCheckExpr env Real b <&> fun b ->
      T (t, Pow (a, b))

  | Arith (op, a, b) ->
      mustBeNumeric t *>
      typeCheckExpr env t a >>= fun a ->
      typeCheckExpr env t b <&> fun b ->
      T (t, Arith (op, a, b))

  | Comp (op, a, b) ->
      mustBe t Bool *>
      typeInferExpr env a >>= fun a ->
      mustNotBeArray "compared" (ty a) *>
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
  | RealLit r -> Ok <| T (Real, RealLit r)
  | TextLit t -> Ok <| T (Text, TextLit t)

  | ArrayLit [] -> Error "Cannot infer the type of empty array literals; Please add a type annotation"
  | ArrayLit es ->
      traverse (typeInferExpr env) es >>= function
        | (T (t, _) :: ts) as es when List.forall (fun e -> ty e = t) ts ->
            Ok <| T (Array t, ArrayLit es)
        | _ -> Error "Mismatched types in array literal"

  | Subscript (a, i) ->
      typeInferExpr env a >>= fun a ->
        match ty a with
        | Array t ->
            typeCheckExpr env Int i <&> fun i ->
            T (t, Subscript (a, i))
        | _ -> Error "Cannot subscript into non-array values"

  | Var i -> lookupVar i env <&> fun t -> T (t, Var i)

  | Not e ->
      typeCheckExpr env Bool e <&> fun e ->
      T (Bool, Not e)

  | Negate e ->
      typeInferExpr env e >>= fun e ->
      mustBeNumeric (ty e) &>
      T (ty e, Negate e)

  | Append (a, b) ->
      typeInferExpr env a >>= fun a ->
      let t = ty a
      mustBeAppendable t *>
      typeCheckExpr env t b <&> fun b ->
      T (t, Append (a, b))

  | Pow (a, b) ->
      typeCheckExpr env Real a >>= fun a ->
      typeCheckExpr env Real b <&> fun b ->
      T (Real, Pow (a, b))

  | Arith (op, a, b) ->
      typeInferExpr env a >>= fun a ->
      let t = ty a
      mustBeNumeric t *>
      typeCheckExpr env t b <&> fun b ->
      T (t, Arith (op, a, b))

  | Comp (op, a, b) ->
      typeInferExpr env a >>= fun a ->
      let t = ty a
      mustNotBeArray "compared" t *>
      typeCheckExpr env t b <&> fun b ->
      T (Bool, Comp (op, a, b))

  | Logic (op, a, b) ->
      typeCheckExpr env Bool a >>= fun a ->
      typeCheckExpr env Bool b <&> fun b ->
      T (Bool, Logic (op, a, b))

let rec typeCheckStmt env stmt =
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

  | Read e ->
      mustBeLValue e *>
      typeInferExpr env e >>= fun e ->
      mustNotBeArray "read" (ty e) &>
      (env, Read e)

  | Write es -> traverse (typeInferExpr env) es <&> fun es -> (env, Write es)

  | If (c, then', else') ->
      typeCheckExpr env Bool c >>= fun c ->
      typeCheckStmts env then' >>= fun (_, then') ->
      typeCheckStmts env else' >>= fun (_, else') ->
      Ok (env, If (c, then', else'))

  | While (c, s) ->
      typeCheckExpr env Bool c >>= fun c ->
      typeCheckStmts env s <&> fun (_, s) -> (env, While (c, s))

  | For (i, _, _, _) when Map.containsKey i env -> Error "For loop counter must be a new variable"
  | For (i, a, b, s) ->
      typeCheckExpr env Int a >>= fun a ->
      typeCheckExpr env Int b >>= fun b ->
      typeCheckStmts (Map.add i Int env) s <&> fun (_, s) ->
      (env, For (i, a, b, s))

and typeCheckStmts env = function
  | [] -> Ok (env, [])
  | stmt :: stmts ->
      typeCheckStmt env stmt >>= fun (env, stmt) ->
      typeCheckStmts env stmts <&> fun (env, stmts) -> (env, stmt :: stmts)

let typeCheckProgram stmts = typeCheckStmts Map.empty stmts <&> snd
