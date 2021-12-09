module Language.Pseudocode.TypeChecker

open Utils.Function
open Utils.Monad.Result
open Language.Pseudocode.Syntax

let lookupVar i env =
  Map.tryFind i env
  |> explain ("Undeclared variable \"" + i + "\"")

let rec typeCheckExpr env expr =
  match expr with
  | IntLit _ -> Ok Int
  | RealLit _ -> Ok Real
  | TextLit _ -> Ok Text
  | BoolLit _ -> Ok Bool
  | ArrayLit es ->
      traverse (typeCheckExpr env) es <&> List.distinct >>= function
        | [t] -> Ok t
        | [] -> Error "Cannot infer the type of empty array literals"
        | _ -> Error "Array literal has mismatched element types"

  | Subscript (a, i) ->
      typeCheckExpr env a >>= function
        | Array t ->
            typeCheckExpr env i >>= function
              | Int -> Ok t
              | _ -> Error "Subscript index is not an Int"
        | _ -> Error "Subscript base is not an array"

  | Var i -> lookupVar i env
  | UnaryOp (op, e) ->
      typeCheckExpr env e >>= fun t ->
        match (op, t) with
        | (Not, Bool) -> Ok Bool
        | (Neg, Int) -> Ok Int
        | (Neg, Real) -> Ok Real
        | (_, _) -> Error "Invalid unary op"

  | BinaryOp (a, op, b) ->
      typeCheckExpr env a >>= fun a ->
      typeCheckExpr env b >>= fun b ->

      let numeric a = List.contains a [Int; Real]
      let arithmetic op = List.contains op [Add; Sub; Mul; Div; Mod; Pow]
      let comparison op = List.contains op [Eq; Neq; Lt; Lte; Gt; Gte]

      match op, a, b with
      | op, a, b when a = b && numeric a && arithmetic op -> Ok a
      | Add, Text, Text -> Ok Text
      | op, a, b when a = b && comparison op -> Ok Bool
      | op, Bool, Bool when List.contains op [And; Or] -> Ok Bool
      | _ -> Error "Invalid binary op"

let mustBe a b =
  if a = b then
    Ok ()
  else
    Error $"Expected {a}, but found {b}"

let rec typeCheckLValue env expr =
  match expr with
  | Var i -> lookupVar i env
  | Subscript (a, i) ->
      typeCheckLValue env a >>= function
        | Array t ->
            typeCheckExpr env i >>= function
              | Int -> Ok t
              | _ -> Error "Subscript index is not an Int"
        | _ -> Error "Subscript base is not an array"
  | _ -> Error "Expected lvalue"

let rec typeCheckStmt env stmt =
  match stmt with
  | Let (i, t, e) ->
      typeCheckExpr env e >>= fun t' ->
      let tc = Option.fold (fun _ -> mustBe t') (Ok ()) t

      if Map.containsKey i env then
        Error "Variable already declared"
      else
        tc &> Map.add i t' env

  | Assign (lhs, e) ->
      typeCheckLValue env lhs >>= fun t ->
      typeCheckExpr env e >>= fun t' ->
      mustBe t t' &> env

  | Read e -> typeCheckLValue env e >>= function
      | Array _ -> Error "Cannot read entire array values"
      | _ -> Ok env

  | Write es -> traverse_ (typeCheckExpr env) es &> env

  | If (c, t, e) ->
      let else' = Option.fold (fun _ -> typeCheckStmts env) (Ok env) e
      typeCheckExpr env c >>= function
        | Bool -> typeCheckStmts env t *> else' &> env
        | _ -> Error "If condition is not a boolean"

  | While (c, s) ->
      typeCheckExpr env c >>= function
        | Bool -> typeCheckStmts env s &> env
        | _ -> Error "While condition is not a boolean"

  | For (i, a, b, s) ->
      if Map.containsKey i env then
        Error "For loop counter must be a new variable"
      else
        typeCheckExpr env a >>= fun ta ->
        mustBe ta Int *>

        typeCheckExpr env b >>= fun tb ->
        mustBe tb Int *>

        let env = Map.add i Int env
        typeCheckStmts env s &> env

and typeCheckStmts env = function
  | [] -> Ok env
  | stmt :: stmts -> typeCheckStmt env stmt >>= flip typeCheckStmts stmts

let typeCheckProgram stmts = typeCheckStmts Map.empty stmts &> stmts
