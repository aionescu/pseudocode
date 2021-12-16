module Midend.Core

open Frontend.Syntax

type Idx = int

type Instr =
  | PushBool of bool
  | PushInt of int
  | PushFloat of float
  | PushString of string
  | NewArr of Type

  | LoadVar of Idx
  | SetVar of Idx
  | Dup
  | LoadIndex of Type
  | SetIndex of Type

  | Read of Type
  | Write of Type
  | WriteLine

  | Length of isString: bool

  | Not
  | Negate

  | Append
  | Pow
  | Arith of ArithOp
  | Comp of CompOp * isString: bool

  | If of Instr list * Instr list
  | While of Instr list * Instr list
  | For of Instr list * Instr list * Instr list

  | Break
  | Continue
