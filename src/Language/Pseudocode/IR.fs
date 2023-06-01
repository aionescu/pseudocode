module Language.Pseudocode.IR

open Language.Pseudocode.Syntax

type Instr =
  | PushBool of bool
  | PushInt of int
  | PushFloat of float
  | PushString of string
  | NewList of Ty

  | Let of Id * Ty * Instr list * Instr list
  | LoadVar of Id
  | SetVar of Id
  | LoadArg of int
  | SetArg of int
  | Dup

  | LoadIndex of Ty
  | SetIndex of Ty
  | ListPush of Ty
  | ListPop of Ty

  | Read of Ty
  | Write of Ty
  | WriteLine

  | StrLength
  | ListLength of Ty

  | Not
  | Negate

  | Append
  | Pow
  | Arith of ArithOp
  | Comp of CompOp
  | StrComp of CompOp

  | If of Instr list * Instr list
  | While of Instr list * Instr list
  | DoWhile of Instr list * Instr list
  | For of Id * bool * Instr list * Instr list

  | Break
  | Continue
  | Return
  | Call of Id
