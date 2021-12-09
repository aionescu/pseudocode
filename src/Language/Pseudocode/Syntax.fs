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
  | Add | Sub | Mul | Div | Mod
  | Eq | Neq | Lt | Lte | Gt | Gte
  | And | Or

type Expr =
  | IntLit of int
  | RealLit of float
  | TextLit of string
  | BoolLit of bool
  | ArrayLit of Expr list
  | Subscript of Expr * Expr
  | Var of Id
  | UnaryOp of UnaryOp * Expr
  | BinaryOp of Expr * BinaryOp * Expr

type Stmt =
  | Let of Id * Type option * Expr
  | Assign of Expr * Expr
  | Read of Expr
  | Write of Expr list
  | If of Expr * Stmt * Stmt option
  | While of Expr * Stmt
  | For of Id * Expr * Expr * Stmt
  | Seq of Stmt * Stmt

type Program = Stmt list
