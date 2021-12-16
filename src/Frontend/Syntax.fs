module Frontend.Syntax

open System
type Id = string

type Type =
  | Bool
  | Int
  | Float
  | String
  | Array of Type

let rec showType = function
  | Array t -> "[" + showType t + "]"
  | t -> $"{t}"

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
  | FloatLit of float
  | StringLit of string
  | ArrayLit of 'e list
  | Var of 'id
  | Read of 'e
  | Length of 'e
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
  | Write of 'e list
  | If of 'e * Stmt<'id, 'e> list * Stmt<'id, 'e> list
  | While of 'e * Stmt<'id, 'e> list
  | For of 'id * 'e * 'e * Stmt<'id, 'e> list
  | Break
  | Continue

type Program = Stmt<Id, UExpr> list
