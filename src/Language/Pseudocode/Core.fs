module Language.Pseudocode.Core

open Language.Pseudocode.Syntax

type Instr =
  | PushInt of int
  | PushReal of float
  | PushBool of bool
  | PushText of string
  | NewArr of Type

  | LoadVar of Id
  | SetVar of Id
  | DeclSetVar of Id
  | Dup
  | LoadIndex
  | SetIndex

  | Read of Type
  | Write of Type

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

  | Neg
  | Not

  | And
  | Or

  | If of Instr list * Instr list
  | While of Instr list * Instr list
