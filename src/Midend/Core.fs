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
  | LoadIndex
  | SetIndex

  | Read of Type
  | Write of Type
  | WriteLine

  | Add
  | Sub
  | Mul
  | Div
  | Mod

  | Pow
  | AppendText
  | AppendArray

  | Eq of isText: bool
  | Neq of isText: bool
  | Lt of isText: bool
  | Lte of isText: bool
  | Gt of isText: bool
  | Gte of isText: bool

  | Not
  | Neg

  | If of Instr list * Instr list
  | While of Instr list * Instr list
