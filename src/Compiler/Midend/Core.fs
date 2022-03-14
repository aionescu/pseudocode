module Compiler.Midend.Core

open Compiler.Frontend.Syntax

type Idx = int

type Instr =
  | PushBool of bool
  | PushInt of int
  | PushFloat of float
  | PushString of string
  | NewList of Type

  | LoadVar of Idx
  | SetVar of Idx
  | ClearVar of Idx * Type
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
  | For of Instr * Instr * Instr

  | Break
  | Continue

  | Nop
  | Seq of Instr * Instr
