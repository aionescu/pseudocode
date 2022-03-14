module Compiler.Midend.SanityCheck

open Utils
open Monad.TC
open Compiler.Frontend.Syntax
open Compiler.Frontend.TypeChecker
open Compiler.Midend.Core

type SanityCheckEnv =
  { vars: Type list
    inLoop: bool
    stack: Type list
  }

let lookupVar v =
  asks (fun e -> e.vars) >>= fun vars ->
  List.tryItem v vars
  |> explain $"Variable \"{v}\" not allocated"
  |> lift

let rec sanityCheckInstr instr =
  ask >>= fun { inLoop = inLoop; stack = stack } ->
  match instr, stack with
  | PushBool _, _ -> pure' (Bool :: stack)
  | PushInt _, _ -> pure' (Int :: stack)
  | PushFloat _, _ -> pure' (Float :: stack)
  | PushString _, _ -> pure' (String :: stack)
  | NewList t, stack -> pure' (List t :: stack)
  | LoadVar v, _ -> lookupVar v <&> fun t -> t :: stack

  | SetVar v, t :: stack ->
      lookupVar v >>= fun v ->
      mustBe v t &> stack

  | ClearVar (v, t), [] -> lookupVar v >>= fun v -> mustBe v t &> []
  | Dup, t :: stack -> pure' (t :: t :: stack)

  | LoadIndex t, Int :: List t' :: stack when t = t' -> pure' (t :: stack)
  | SetIndex t, t' :: Int :: List t'' :: stack when t = t' && t = t'' -> pure' stack
  | Push t, t' :: List t'' :: stack when t = t' && t = t'' -> pure' stack
  | Pop t, List t' :: stack when t = t' -> pure' stack

  | Read t, _ -> mustNotBeList "read" t &> t :: stack
  | Write t, t' :: stack when t = t' -> mustNotBeList "written in Core" t &>  stack
  | WriteLine, _ -> pure' stack

  | Length None, String :: stack -> pure' (Int :: stack)
  | Length (Some t), List t' :: stack when t = t' -> pure' (Int :: stack)

  | Not, Bool :: stack -> pure' (Bool :: stack)

  | Negate, Int :: stack -> pure' (Int :: stack)
  | Negate, Float :: stack -> pure' (Float :: stack)

  | Append, String :: String :: stack -> pure' (String :: stack)
  | Pow, Float :: Float :: stack -> pure' (Float :: stack)

  | Arith _, Int :: Int :: stack -> pure' (Int :: stack)
  | Arith _, Float :: Float :: stack -> pure' (Float :: stack)

  | Comp (_, true), String :: String :: stack -> pure' (Bool :: stack)
  | Comp (_, false), t :: t' :: stack when t = t' -> mustNotBeList "compared" t &> Bool :: stack

  | If (t, f), Bool :: stack ->
      ensureEmptyStack inLoop "True branch" t *>
      ensureEmptyStack inLoop "False branch" f &>
      stack

  | While (c, s), _ ->
      withEmptyStack c >>= function
        | [Bool] -> ensureEmptyStack true "While body" s &> stack
        | _ -> err "Invalid While condition"

  | DoWhile (s, c), _ ->
      withEmptyStack c >>= function
        | [Bool] -> ensureEmptyStack true "While body" s &> stack
        | _ -> err "Invalid DoWhile condition"

  | For (c, s, u), _ ->
      withEmptyStack c >>= function
        | [Bool] ->
            ensureEmptyStack true "For body" s *>
            ensureEmptyStack true "For update block" u
            &> stack
        | _ -> err "Invalid For condition"

  | (Break | Continue), [] when inLoop -> pure' []
  | Nop, _ -> pure' stack

  | Seq (t, s), _ ->
      sanityCheckInstr t >>= fun stack' ->
      local (fun e -> { e with stack = stack' }) <| sanityCheckInstr s

  | _ -> err $"Invalid Core instruction:\n{instr}\n{stack}"

and withEmptyStack = local (fun e -> { e with stack = [] }) << sanityCheckInstr

and ensureEmptyStack inLoop label instr =
  withEmptyStack instr
  |> local (fun e -> { e with inLoop = inLoop })
  >>= function
    | [] -> pure' ()
    | _ -> err $"{label} resulted in non-empty stack"

let sanityCheck (vars, instr) =
  ensureEmptyStack false "Program" instr
  &> (vars, instr)
  |> runTC { vars = vars; inLoop = false; stack = [] }
  |> Result.mapError ((+) "Core sanityCheck error: ")
