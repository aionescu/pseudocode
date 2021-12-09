namespace Language.Pseudocode.Syntax

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
  | Add | Sub | Mul | Div | Mod | Pow
  | Eq | Neq | Lt | Lte | Gt | Gte
  | And | Or

type Expr =
  | IntLit of int64
  | RealLit of float
  | TextLit of string
  | BoolLit of bool
  | ArrayLit of Expr list
  | Var of Id
  | Subscript of Expr * Expr
  | UnaryOp of UnaryOp * Expr
  | BinaryOp of Expr * BinaryOp * Expr

type Stmt =
  | Let of Id * Type option * Expr
  | Assign of Expr * Expr
  | Read of Expr
  | Write of Expr list
  | If of Expr * Stmt list * Stmt list option
  | While of Expr * Stmt list
  | For of Id * Expr * Expr * Stmt list

type Program = Stmt list
