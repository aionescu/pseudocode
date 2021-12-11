module Language.Pseudocode.TypeChecker

open Utils.Function
open Utils.Monad.Result
open Language.Pseudocode.Syntax

let lookupVar i env =
  Map.tryFind i env
  |> explain ("Undeclared variable \"" + i + "\"")

let rec typeCheckExpr env expr =
  match unU expr with
  | BoolLit b -> Ok <| T (Bool, BoolLit b)
  | IntLit i -> Ok <| T (Int, IntLit i)
  | RealLit r -> Ok <| T (Real, RealLit r)
  | TextLit t -> Ok <| T (Text, TextLit t)
  | ArrayLit es ->
      traverse (typeCheckExpr env) es >>= fun ts ->
        match List.distinct (List.map ty ts) with
        | [t] -> Ok <| T (Array t, ArrayLit ts)
        | [] -> Error "Cannot infer the type of empty array literals"
        | _ -> Error "Array literal has mismatched element types"

  | Subscript (a, i) ->
      typeCheckExpr env a >>= fun a ->
        match ty a with
        | Array t ->
            typeCheckExpr env i >>= fun i ->
              match ty i with
              | Int -> Ok <| T (t, Subscript (a, i))
              | _ -> Error "Subscript index is not an Int"
        | _ -> Error "Subscript base is not an array"

  | Var i -> lookupVar i env <&> fun t -> T (t, Var i)

  | UnaryOp (op, e) ->
      typeCheckExpr env e >>= fun e ->
        match (op, ty e) with
        | (Not, Bool) -> Ok <| T (Bool, UnaryOp (Not, e))
        | (Neg, Int) -> Ok <| T (Int, UnaryOp (Neg, e))
        | (Neg, Real) -> Ok <| T (Real, UnaryOp (Neg, e))
        | (_, _) -> Error "Invalid unary op"

  | BinaryOp (a, op, b) ->
      let numeric a = List.contains a [Int; Real]
      let primitive = function
        | Array _ -> false
        | _ -> true

      typeCheckExpr env a >>= fun a ->
      typeCheckExpr env b >>= fun b ->

      let te = BinaryOp (a, op, b)

      match op, ty a, ty b with
      | ArithOp, a, b when a = b && numeric a -> Ok <| T (a, te)
      | Pow, Real, Real -> Ok <| T (Real, te)
      | Append, Text, Text -> Ok <| T (Text, te)
      | Append, Array a, Array b when a = b -> Ok <| T (Array a, te)
      | CompOp, a, b when a = b && primitive a -> Ok <| T (Bool, te)
      | LogicOp, Bool, Bool -> Ok <| T (Bool, te)
      | _ -> Error "Invalid binary op"

let mustBe a b =
  if a = b then
    Ok ()
  else
    Error $"Expected {a}, but found {b}"

let rec typeCheckLValue env expr =
  match unU expr with
  | Var i -> lookupVar i env <&> fun t -> T (t, Var i)
  | Subscript (a, i) ->
      typeCheckLValue env a >>= fun a ->
        match ty a with
        | Array t ->
            typeCheckExpr env i >>= fun i ->
              match ty i with
              | Int -> Ok <| T (t, Subscript (a, i))
              | _ -> Error "Subscript index is not an Int"
        | _ -> Error "Subscript base is not an array"
  | _ -> Error "Expected lvalue"

let rec typeCheckStmt env stmt =
  match stmt with
  | Let (i, t, e) ->
      typeCheckExpr env e >>= fun e ->
      let te = ty e
      let tc = Option.fold (fun _ -> mustBe te) (Ok ()) t

      if Map.containsKey i env then
        Error "Variable already declared"
      else
        tc &> (Map.add i te env, Let (i, Some te, e))

  | Assign (lhs, e) ->
      typeCheckLValue env lhs >>= fun lhs ->
      typeCheckExpr env e >>= fun e ->
      mustBe (ty lhs) (ty e) &> (env, Assign (lhs, e))

  | Read e ->
      typeCheckLValue env e >>= fun e ->
      match ty e with
      | Array _ -> Error "Cannot read array values"
      | _ -> Ok (env, Read e)

  | Write es -> traverse (typeCheckExpr env) es <&> fun es -> (env, Write es)

  | If (c, then', else') ->
      typeCheckExpr env c >>= fun c ->
      match ty c with
      | Bool ->
          typeCheckStmts env then' >>= fun (_, then') ->
          typeCheckStmts env else' >>= fun (_, else') ->
          Ok (env, If (c, then', else'))
      | _ -> Error "If condition is not a boolean"

  | While (c, s) ->
      typeCheckExpr env c >>= fun c ->

      match ty c with
      | Bool -> typeCheckStmts env s <&> fun (_, s) -> (env, While (c, s))
      | _ -> Error "While condition is not a boolean"

  | For (i, a, b, s) ->
      if Map.containsKey i env then
        Error "For loop counter must be a new variable"
      else
        typeCheckExpr env a >>= fun a ->
        mustBe (ty a) Int *>

        typeCheckExpr env b >>= fun b ->
        mustBe (ty b) Int *>

        let env = Map.add i Int env
        typeCheckStmts env s <&> fun (_, s) -> (env, For (i, a, b, s))

and typeCheckStmts env : Stmt<UExpr> list -> Result<Map<Id, Type> * Stmt<TExpr> list, string> = function
  | [] -> Ok (env, [])
  | stmt :: stmts ->
      typeCheckStmt env stmt >>= fun (env, stmt) ->
      typeCheckStmts env stmts <&> fun (env, stmts) -> (env, stmt :: stmts)

and typeCheckStmts' env stmts =
  typeCheckStmts env stmts <&> fun (_, stmts) -> (env, stmts)

let typeCheckProgram stmts = typeCheckStmts Map.empty stmts <&> snd
