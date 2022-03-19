module Compiler.IR

open Compiler.AST

type Instr =
  | PushBool of bool
  | PushInt of int
  | PushFloat of float
  | PushString of string
  | NewList of Ty

  | Let of Id * Ty * Instr * Instr
  | LoadVar of Id
  | SetVar of Id
  | Dup

  | LoadIndex of Ty
  | SetIndex of Ty
  | Push of Ty
  | Pop of Ty

  | Read of Ty
  | Write of Ty
  | WriteLine

  | Length of Ty option

  | Not
  | Negate

  | Append
  | Pow
  | Arith of ArithOp
  | Comp of CompOp * isString: bool

  | If of Instr * Instr
  | While of Instr * Instr
  | DoWhile of Instr * Instr
  | For of Id * Instr * bool * Instr * Instr

  | Break
  | Continue
  | Return
  | Call of Id

  | Seq of Instr * Instr
  | Nop
