module Frontend.Syntax

open System
type Id = string

type Type =
  | Int
  | Real
  | Text
  | Bool
  | Array of Type

let rec showType = function
  | Int -> "Integer"
  | Real -> "Real"
  | Text -> "Text"
  | Bool -> "Boolean"
  | Array t -> "[" + showType t + "]"

type ArithOp =
  | Add
  | Sub
  | Mul
  | Div
  | Mod

type CompOp =
  | Eq
  | Neq
  | Lt
  | Lte
  | Gt
  | Gte

type LogicOp =
  | And
  | Or

type Expr<'id, 'e> =
  | BoolLit of bool
  | IntLit of int
  | RealLit of float
  | TextLit of string
  | ArrayLit of 'e list
  | Var of 'id
  | Subscript of 'e * 'e
  | Not of 'e
  | Negate of 'e
  | Append of 'e * 'e
  | Pow of 'e * 'e
  | Arith of ArithOp * 'e * 'e
  | Comp of CompOp * 'e * 'e
  | Logic of LogicOp * 'e * 'e

type UExpr = U of Expr<Id, UExpr>
type TExpr<'id> = T of Type * Expr<'id, TExpr<'id>>

let unU (U e) = e
let unT (T (t, e)) = (t, e)
let ty (T (t, _)) = t
let ex (T (_, e)) = e
let mapEx f (T (t, e)) = T (t, f e)

type Stmt<'id, 'e> =
  | Let of 'id * Type option * 'e
  | Assign of 'e * 'e
  | Read of 'e
  | Write of 'e list
  | If of 'e * Stmt<'id, 'e> list * Stmt<'id, 'e> list
  | While of 'e * Stmt<'id, 'e> list
  | For of 'id * 'e * 'e * Stmt<'id, 'e> list

type Program = Stmt<Id, UExpr> list

let showArithOp = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"

let showCompOp = function
  | Eq -> "=="
  | Neq -> "!="
  | Lt -> "<"
  | Lte -> "<="
  | Gt -> ">"
  | Gte -> ">="

let showLogicOp = function
  | And -> "and"
  | Or -> "or"

let rec showUExpr (U expr) =
  match expr with
  | BoolLit b -> b.ToString()
  | IntLit i -> string i
  | RealLit r -> string r
  | TextLit t -> $"\"{t}\""
  | ArrayLit es -> "[" + String.concat ", " (List.map showUExpr es) + "]"
  | Var i -> i
  | Subscript (a, i) -> $"{showUExpr a}[{showUExpr i}]"
  | Not e -> $"(not {showUExpr e})"
  | Negate e -> $"(-{showUExpr e})"
  | Append (a, b) -> $"({showUExpr a} <> {showUExpr b})"
  | Pow (a, b) -> $"({showUExpr a} ^ {showUExpr b})"
  | Arith (op, a, b) -> $"({showUExpr a} {showArithOp op} {showUExpr b})"
  | Comp (op, a, b) -> $"({showUExpr a} {showCompOp op} {showUExpr b})"
  | Logic (op, a, b) -> $"({showUExpr a} {showLogicOp op} {showUExpr b})"

let join indent = List.map ((+) indent) >> String.concat "\n"

let rec showStmt = function
  | Let (i, None, e) -> [$"let {i} = {showUExpr e}"]
  | Let (i, Some t, e) -> [$"let {i}: {showType t} = {showUExpr e}"]
  | Assign (i, e) -> [$"{i} := {showUExpr e}"]
  | Read e -> [$"read {showUExpr e}"]
  | Write es -> ["write " + String.concat ", " (List.map showUExpr es)]

  | If (c, t, e) ->
      [ $"if {showUExpr c} then"
        yield! showNested t
        "else"
        yield! showNested e
        "end"
      ]

  | While (c, s) ->
      [ $"while {showUExpr c} do"
        yield! showNested s
        "end"
      ]

  | For (i, a, b, s) ->
      [ $"for {i} = {showUExpr a} to {showUExpr b} do"
        yield! showNested s
        "end"
      ]

and showNested stmts =
  stmts
  |> List.collect showStmt
  |> List.map ((+) "  ")

let showProgram stmts =
  stmts
  |> List.collect showStmt
  |> String.concat "\n"
