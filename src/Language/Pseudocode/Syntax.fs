module Language.Pseudocode.Syntax

type Id = string

type Type =
  | Int
  | Real
  | Text
  | Bool
  | Array of Type

type UnaryOp =
  | Not
  | Neg

type BinaryOp =
  | Add | Sub | Mul | Div | Mod
  | Pow
  | Append
  | Eq | Neq | Lt | Lte | Gt | Gte
  | And | Or

let (|ArithOp|Pow|Append|CompOp|LogicOp|) = function
  | Add | Sub | Mul | Div | Mod -> ArithOp
  | Pow -> Pow
  | Append -> Append
  | Eq | Neq | Lt | Lte | Gt | Gte -> CompOp
  | And | Or -> LogicOp

type 'e Expr =
  | BoolLit of bool
  | IntLit of int
  | RealLit of float
  | TextLit of string
  | ArrayLit of 'e list
  | Var of Id
  | Subscript of 'e * 'e
  | UnaryOp of UnaryOp * 'e
  | BinaryOp of 'e * BinaryOp * 'e

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
