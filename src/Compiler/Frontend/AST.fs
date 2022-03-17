module Compiler.Frontend.AST

open Utils
open Monad.TC

type Id = string

type Type =
  | Bool
  | Int
  | Float
  | String
  | List of Type

let rec showType = function
  | List t -> "[" + showType t + "]"
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

type Expr<'e> =
  | BoolLit of bool
  | IntLit of int
  | FloatLit of float
  | StringLit of string
  | ListLit of 'e list
  | Var of Id
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
  | FnCall of Id * 'e list

type UExpr = U of Expr<UExpr>
type TExpr = T of Type * Expr<TExpr>

let unU (U e) = e
let unT (T (t, e)) = (t, e)
let ty (T (t, _)) = t
let ex (T (_, e)) = e
let mapEx f (T (t, e)) = T (t, f e)

type Stmt<'e> =
  | Let of Id * Type option * 'e * Stmt<'e>
  | Assign of 'e * 'e
  | Push of 'e * 'e list
  | Pop of 'e
  | Write of 'e list
  | If of 'e * Stmt<'e> * Stmt<'e>
  | While of 'e * Stmt<'e>
  | DoWhile of Stmt<'e> * 'e
  | For of Id * 'e * bool * 'e * Stmt<'e>
  | Break
  | Continue
  | Return of 'e option
  | FnCallStmt of Id * 'e list
  | Seq of Stmt<'e> * Stmt<'e>
  | Nop

type FnSig =
  { name: Id
    args: (Id * Type) list
    retType: Type option
  }

type Program<'b> =
  { fns: Map<Id, FnSig * 'b>
  }

let mapFns f { fns = fns } =
  { fns = mapVals (fun (fnSig, body) -> fnSig, f fnSig body) fns }

let traverseFns f { fns = fns } =
  fns
  |> traverseMap (fun (fnSig, body) -> pair fnSig <!> f fnSig body)
  <&> fun fns -> { fns = fns }
