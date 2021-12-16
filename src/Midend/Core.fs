module Midend.Core

open Frontend.Syntax

type Idx = int

type Instr =
  | PushInt of int
  | PushReal of float
  | PushBool of bool
  | PushText of string
  | NewArr of Type

  | LoadVar of Idx
  | SetVar of Idx
  | Dup
  | LoadIndex of Type
  | SetIndex of Type

  | Read of Type
  | Write of Type
  | WriteLine

  | Length of isText: bool

  | Not
  | Negate

  | Append
  | Pow
  | Arith of ArithOp
  | Comp of CompOp * isText: bool

  | If of Instr list * Instr list
  | While of Instr list * Instr list
