module Language.Pseudocode.Syntax

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

type 'e Expr =
  | BoolLit of bool
  | IntLit of int
  | RealLit of float
  | TextLit of string
  | ArrayLit of 'e list
  | Var of Id
  | Subscript of 'e * 'e
  | Not of 'e
  | Negate of 'e
  | Append of 'e * 'e
  | Pow of 'e * 'e
  | Arith of ArithOp * 'e * 'e
  | Comp of CompOp * 'e * 'e
  | Logic of LogicOp * 'e * 'e

type UExpr = U of UExpr Expr
type TExpr = T of Type * TExpr Expr

let unU (U e) = e
let unT (T (t, e)) = (t, e)
let ty (T (t, _)) = t
let ex (T (_, e)) = e

type 'e Stmt =
  | Let of Id * Type option * 'e
  | Assign of 'e * 'e
  | Read of 'e
  | Write of 'e list
  | If of 'e * 'e Stmt list * 'e Stmt list
  | While of 'e * 'e Stmt list
  | For of Id * 'e * 'e * 'e Stmt list

type Program = UExpr Stmt list
