module Midend.CoreTypeChecker

open Utils.Monad.Result
open Frontend.Syntax
open Frontend.TypeChecker
open Midend.Core
open Utils.Misc

let lookupVar v env =
  List.tryItem v env
  |> explain $"Variable \"{v}\" not allocated"

let rec typeCheckInstr env inLoop stack instr =
  match instr, stack with
  | PushBool _, _ -> Ok (Bool :: stack)
  | PushInt _, _ -> Ok (Int :: stack)
  | PushFloat _, _ -> Ok (Float :: stack)
  | PushString _, _ -> Ok (String :: stack)
  | NewList t, stack -> Ok (List t :: stack)
  | LoadVar v, _ -> lookupVar v env <&> fun t -> t :: stack

  | SetVar v, t :: stack ->
      lookupVar v env >>= fun v ->
      mustBe v t &> stack

  | Dup, t :: stack -> Ok (t :: t :: stack)

  | LoadIndex t, Int :: List t' :: stack when t = t' -> Ok (t :: stack)
  | SetIndex t, t' :: Int :: List t'' :: stack when t = t' && t = t'' -> Ok stack
  | Push t, t' :: List t'' :: stack when t = t' && t = t'' -> Ok stack
  | Pop t, List t' :: stack when t = t' -> Ok stack

  | Read t, _ -> mustNotBeList "read" t &> t :: stack
  | Write t, t' :: stack when t = t' -> mustNotBeList "written in Core" t &>  stack
  | WriteLine, _ -> Ok stack

  | Length None, String :: stack -> Ok (Int :: stack)
  | Length (Some t), List t' :: stack when t = t' -> Ok (Int :: stack)

  | Not, Bool :: stack -> Ok (Bool :: stack)

  | Negate, Int :: stack -> Ok (Int :: stack)
  | Negate, Float :: stack -> Ok (Float :: stack)

  | Append, String :: String :: stack -> Ok (String :: stack)
  | Pow, Float :: Float :: stack -> Ok (Float :: stack)

  | Arith _, Int :: Int :: stack -> Ok (Int :: stack)
  | Arith _, Float :: Float :: stack -> Ok (Float :: stack)

  | Comp (_, true), String :: String :: stack -> Ok (Bool :: stack)
  | Comp (_, false), t :: t' :: stack when t = t' -> mustNotBeList "compared" t &> Bool :: stack

  | If (t, f), Bool :: stack ->
      typeCheckEmpty env inLoop "True branch" t *>
      typeCheckEmpty env inLoop "False branch" f &>
      stack

  | While (c, s), _ ->
      typeCheckInstrs env inLoop [] c >>= function
        | [Bool] -> typeCheckEmpty env true "While body" s &> stack
        | _ -> Error "Invalid While condition"

  | DoWhile (s, c), _ ->
      typeCheckInstrs env inLoop [] c >>= function
        | [Bool] -> typeCheckEmpty env true "While body" s &> stack
        | _ -> Error "Invalid DoWhile condition"

  | For (c, s, u), _ ->
      typeCheckInstrs env inLoop [] c >>= function
        | [Bool] ->
            typeCheckEmpty env true "For body" s *>
            typeCheckEmpty env true "For update block" u
            &> stack
        | _ -> Error "Invalid For condition"

  | (Break | Continue), [] when inLoop -> Ok []

  | _ -> Error $"Invalid Core instruction:\n{instr}\n{stack}"

and typeCheckInstrs env inLoop stack = function
  | [] -> Ok stack
  | instr :: instrs -> typeCheckInstr env inLoop stack instr >>= flip (typeCheckInstrs env inLoop) instrs

and typeCheckEmpty env inLoop label instrs =
  typeCheckInstrs env inLoop [] instrs >>= function
    | [] -> Ok ()
    | _ -> Error $"{label} resulted in non-empty stack"

let typeCheckCore (vars, instrs) =
  typeCheckEmpty vars false "Program" instrs
  &> (vars, instrs)
  |> Result.mapError ((+) "Core type error: ")
