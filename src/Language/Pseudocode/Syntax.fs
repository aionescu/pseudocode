module Language.Pseudocode.Syntax

open Utils
open Control.Monad.TC

type Id = string

type Ty =
  | Bool
  | Int
  | Float
  | String
  | List of Ty

let rec showTy = function
  | List t -> "[" + showTy t + "]"
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

[<Struct>] type UExpr = U of Expr<UExpr>
[<Struct>] type TExpr = T of Ty * Expr<TExpr>

let ty (T (t, _)) = t
let ex (T (_, e)) = e

type Stmt<'e> =
  | Let of Id * Ty option * 'e * Stmt<'e>
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
    args: (Id * Ty) list
    retTy: Ty option
  }

type Program<'b> =
  { fns: Map<Id, FnSig * 'b>
  }

let mapVals f = Map.map (const' f)

let mapFns f { fns = fns } =
  { fns = mapVals (fun (fnSig, body) -> fnSig, f fnSig body) fns }

let traverseFns f { fns = fns } =
  fns
  |> traverseMap (fun (fnSig, body) -> pair fnSig <!> f fnSig body)
  <&> fun fns -> { fns = fns }
