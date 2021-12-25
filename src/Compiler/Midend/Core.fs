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

  | If of Instr list * Instr list
  | While of Instr list * Instr list
  | DoWhile of Instr list * Instr list
  | For of Instr list * Instr list * Instr list

  | Break
  | Continue
