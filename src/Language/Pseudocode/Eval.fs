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

let panic () = failwith "Panic in Eval"

let rec evalExpr env (T (_, expr)) =
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

  | Not e ->
      match evalExpr env e with
      | VBool b -> VBool (not b)
      | _ -> panic ()

  | Negate e ->
      match evalExpr env e with
      | VInt i -> VInt -i
      | VReal r -> VReal -r
      | _ -> panic ()

  | Append (a, b) ->
      match evalExpr env a, evalExpr env b with
      | VText a, VText b -> VText (a + b)
      | VArray a, VArray b -> VArray <| Array.append a b
      | _ -> panic ()

  | Pow (a, b) ->
      match evalExpr env a, evalExpr env b with
      | VReal a, VReal b -> VReal (a ** b)
      | _ -> panic ()

  | Arith (op, a, b) ->
      let inline eval op =
        match op with
        | Add -> (+)
        | Sub -> (-)
        | Mul -> (*)
        | Div -> (/)
        | Mod -> (%)

      match evalExpr env a, evalExpr env b with
      | VInt a, VInt b -> VInt (eval op a b)
      | VReal a, VReal b -> VReal (eval op a b)
      | _ -> panic ()

  | Comp (op, a, b) ->
      let (><) =
        match op with
        | Eq -> (=)
        | Neq -> (<>)
        | Lt -> (<)
        | Lte -> (<=)
        | Gt -> (>)
        | Gte -> (>=)

      VBool (evalExpr env a >< evalExpr env b)

  | Logic (op, a, b) ->
      match op, evalExpr env a with
      | And, VBool false -> VBool false
      | Or, VBool true -> VBool true
      | _ -> evalExpr env b

let assign env (T (_, e)) v =
  match e with
  | Var i -> Map.add i v env
  | Subscript (a, i) ->
      match evalExpr env a, evalExpr env i with
      | VArray a, VInt i -> a[i] <- v; env
      | _ -> panic ()
  | _ -> panic ()

let rec evalStmt env stmt =
  match stmt with
  | Let (v, _, e) -> Map.add v (evalExpr env e) env
  | Assign (v, e) -> assign env v (evalExpr env e)

  | Read e ->
      let line = Console.ReadLine()
      assign env e <|
        match ty e with
        | Int -> VInt <| int line
        | Real -> VReal <| float line
        | Bool -> VBool <| Boolean.Parse(line)
        | Text -> VText line
        | _ -> panic ()

  | Write es -> Console.WriteLine(String.Join(" ", List.map (evalExpr env >> showVal) es)); env

  | If (c, t, e) ->
      match evalExpr env c with
      | VBool true -> evalStmts env t
      | VBool false -> evalStmts env e
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

      let var = T (ty b, Var i)

      go i b
        (s @ [Assign (var, T <| (Int, Arith (Add, var, T (Int, IntLit 1))))])
        (Map.add i (evalExpr env a) env)

and evalStmts env = function
  | [] -> env
  | stmt :: stmts -> evalStmt env stmt |> flip evalStmts stmts

let evalProgram stmts = ignore <| evalStmts Map.empty stmts
