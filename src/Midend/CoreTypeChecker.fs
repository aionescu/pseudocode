module Midend.CoreTypeChecker

open Utils.Result
open Frontend.Syntax
open Frontend.TypeChecker
open Midend.Core
open Utils.Function

let lookupVar v env =
  List.tryItem v env
  |> explain $"Variable \"{v}\" not allocated"

let rec typeCheckInstr env stack instr =
  match instr, stack with
  | PushInt _, _ -> Ok (Int :: stack)
  | PushReal _, _ -> Ok (Real :: stack)
  | PushBool _, _ -> Ok (Bool :: stack)
  | PushText _, _ -> Ok (Text :: stack)
  | NewArr t, Int :: stack -> Ok (Array t :: stack)
  | LoadVar v, _ -> lookupVar v env <&> fun t -> t :: stack

  | SetVar v, t :: stack ->
      lookupVar v env >>= fun v ->
      mustBe v t &> stack

  | Dup, t :: stack -> Ok (t :: t :: stack)
  | LoadIndex t, Int :: Array t' :: stack when t = t' -> Ok (t :: stack)
  | SetIndex t, t' :: Int :: Array t'' :: stack when t = t' && t = t'' -> Ok stack
  | Read t, _ -> mustNotBeArray "read" t &> t :: stack
  | Write t, t' :: stack when t = t' -> mustNotBeArray "written in Core" t &>  stack
  | WriteLine, _ -> Ok stack

  | Length true, Text :: stack  -> Ok (Int :: stack)
  | Length false, Array _ :: stack -> Ok (Int :: stack)

  | Not, Bool :: stack -> Ok (Bool :: stack)

  | Negate, Int :: stack -> Ok (Int :: stack)
  | Negate, Real :: stack -> Ok (Real :: stack)

  | Append, Text :: Text :: stack -> Ok (Text :: stack)
  | Pow, Real :: Real :: stack -> Ok (Real :: stack)

  | Arith _, Int :: Int :: stack -> Ok (Int :: stack)
  | Arith _, Real :: Real :: stack -> Ok (Real :: stack)

  | Comp (_, true), Text :: Text :: stack -> Ok (Bool :: stack)
  | Comp (_, false), t :: t' :: stack when t = t' -> mustNotBeArray "compared" t &> Bool :: stack

  | If (t, f), Bool :: stack ->
      typeCheckEmpty env "True branch" t *>
      typeCheckEmpty env "False branch" f &>
      stack

  | While (c, s), _ ->
      typeCheckInstrs env [] c >>= function
        | [Bool] -> typeCheckEmpty env "While body" s &> stack
        | _ -> Error "Invalid While condition"

  | _ -> Error "Invalid Core instruction"

and typeCheckInstrs env stack = function
  | [] -> Ok stack
  | instr :: instrs -> typeCheckInstr env stack instr >>= flip (typeCheckInstrs env) instrs

and typeCheckEmpty env label instrs =
  typeCheckInstrs env [] instrs >>= function
    | [] -> Ok ()
    | _ -> Error $"{label} resulted in non-empty stack"

let typeCheckCore env instrs = typeCheckEmpty env "Program" instrs &> instrs
