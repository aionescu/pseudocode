module Language.Pseudocode.Eval

open System

open Utils.Function
open Language.Pseudocode.Syntax

type Val =
  | VBool of bool
  | VInt of int
  | VReal of float
  | VText of string
  | VArray of Val array

let rec showVal = function
  | VBool b -> string b
  | VInt i -> string i
  | VReal r -> string r
  | VText s -> s
  | VArray vs -> "[" + String.Join(", ", Array.map showVal vs) + "]"

let panic () = failwith "Runtime error. Did you run the typechecker?"

let rec evalExpr env expr =
  match expr with
  | BoolLit b -> VBool b
  | IntLit i -> VInt i
  | RealLit r -> VReal r
  | TextLit s -> VText s
  | ArrayLit es -> List.map (evalExpr env) es |> List.toArray |> VArray
  | Var v -> Map.find v env

  | Subscript (e, i) ->
      match evalExpr env e, evalExpr env i with
      | VArray a, VInt i -> a[i]
      | _ -> panic ()

  | UnaryOp (op, e) ->
      match op, evalExpr env e with
      | Not, VBool b -> VBool <| not b
      | Neg, VInt i -> VInt -i
      | Neg, VReal r -> VReal -r
      | _ -> panic ()

  | BinaryOp (a, op, b) ->
      match op with
      | ArithmeticOp -> evalArithmeticOp env op a b
      | ComparisonOp -> evalComparisonOp env op a b
      | LogicOp -> evalLogicOp env op a b

and evalArithmeticOp env op a b =
  let inline evalOp op =
    match op with
    | Add -> (+)
    | Sub -> (-)
    | Mul -> (*)
    | Div -> (/)
    | Mod -> (%)
    | _ -> panic ()

  match op, evalExpr env a, evalExpr env b with
  | Add, VText a, VText b -> VText <| a + b
  | Add, VArray a, VArray b -> VArray <| Array.append a b
  | _, VInt a, VInt b -> VInt (evalOp op a b)
  | _, VReal a, VReal b -> VReal (evalOp op a b)
  | _ -> panic ()

and evalComparisonOp env op a b =
  let (><) =
    match op with
    | Eq -> (=)
    | Neq -> (<>)
    | Lt -> (<)
    | Lte -> (<=)
    | Gt -> (>)
    | Gte -> (>=)
    | _ -> panic ()

  VBool <| (evalExpr env a >< evalExpr env b)

and evalLogicOp env op a b =
  match op, evalExpr env a with
  | And, VBool false -> VBool false
  | And, _ -> evalExpr env b

  | Or, VBool true -> VBool true
  | Or, _ -> evalExpr env b

  | _ -> panic ()

let assign env e v =
  let unArray = function
    | VArray a -> a
    | _ -> panic ()

  let unInt = function
    | VInt i -> i
    | _ -> panic ()

  let rec getArray = function
    | Var a -> unArray <| Map.find a env
    | Subscript (a, i) -> unArray <| (getArray a)[unInt <| evalExpr env i]
    | _ -> panic ()

  match e with
  | Var i -> Map.add i v env
  | Subscript (a, i) -> (getArray a)[unInt <| evalExpr env i] <- v; env
  | _ -> panic ()

let rec evalStmt env stmt =
  match stmt with
  | Assign (v, e) -> assign env v (evalExpr env e)
  | Let (v, _, e) -> Map.add v (evalExpr env e) env

  | Read e ->
      let line = Console.ReadLine()
      let v =
        match evalExpr env e with
        | VInt _ -> VInt <| int line
        | VReal _ -> VReal <| float line
        | VBool _ -> VBool <| Boolean.Parse(line)
        | VText _ -> VText line
        | _ -> panic ()

      assign env e v

  | Write es -> Console.WriteLine(String.Join(" ", List.map (evalExpr env >> showVal) es)); env

  | If (c, t, e) ->
      match evalExpr env c with
      | VBool true -> evalStmts env t
      | VBool false -> evalStmts env (defaultArg e [])
      | _ -> panic ()

  | While (c, s) as w ->
      match evalExpr env c with
      | VBool true -> evalStmts env s |> flip evalStmt w
      | VBool false -> env
      | _ -> panic ()

  | For (i, a, b, s) ->
      let rec go i b s env =
        match Map.find i env, evalExpr env b with
        | VInt i, VInt b when i > b -> env
        | _ -> evalStmts env s |> go i b s

      go i b
        (List.append s [Assign (Var i, BinaryOp (Var i, Add, IntLit 1))])
        (Map.add i (evalExpr env a) env)

and evalStmts env = function
  | [] -> env
  | stmt :: stmts -> evalStmt env stmt |> flip evalStmts stmts

let evalProgram stmts = ignore <| evalStmts Map.empty stmts
