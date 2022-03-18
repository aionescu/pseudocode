module Compiler.Midend.IR

open Compiler.Frontend.AST

type Instr =
  | PushBool of bool
  | PushInt of int
  | PushFloat of float
  | PushString of string
  | NewList of Type

  | Let of Id * Type * Instr * Instr
  | LoadVar of Id
  | SetVar of Id
  | Dup

  | LoadIndex of Type
  | SetIndex of Type
  | Push of Type
  | Pop of Type

  | Read of Type
  | Write of Type
  | WriteLine

  | Length of Type option

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
