module Compiler.Midend.SanityCheck

open Utils
open Monad.TC
open Compiler.Frontend.AST
open Compiler.Frontend.TypeChecker
open Compiler.Midend.IR

type SanityCheckEnv =
  { fns: Map<Id, FnSig>
    vars: Type list
    args: Type list
    retType: Type option
    inLoop: bool
    stack: Type list
  }

let lookupVar v =
  asks (fun e -> List.tryItem v e.vars)
  >>= (explain $"Variable \"{v}\" not allocated" >> lift)

let lookupArg v =
  asks (fun e -> List.tryItem v e.args)
  >>= (explain $"Argument \"{v}\" does not exist" >> lift)

let sanityCheckCall f stack =
  asks (fun e -> e.fns[f]) >>= fun { args = args; retType = retType } ->
  let argc = List.length args
  let args = List.map snd args
  let stackArgs = List.rev <| List.take argc stack

  if stackArgs <> args then
    err $"Invalid args in Call \"${f}\": Expected {args}, found {stackArgs}"
  else
    pure' <| Option.toList retType @ List.skip argc stack

let rec sanityCheckInstr instr =
  ask >>= fun { retType = retType; inLoop = inLoop; stack = stack } ->
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

  | LoadArg v, _ -> lookupArg v <&> fun t -> t :: stack

  | SetArg v, t :: stack ->
      lookupArg v >>= fun v ->
      mustBe v t &> stack

  | LoadIndex t, Int :: List t' :: stack when t = t' -> pure' (t :: stack)
  | SetIndex t, t' :: Int :: List t'' :: stack when t = t' && t = t'' -> pure' stack
  | Push t, t' :: List t'' :: stack when t = t' && t = t'' -> pure' stack
  | Pop t, List t' :: stack when t = t' -> pure' stack

  | Read t, _ -> mustNotBeList "read" t &> t :: stack
  | Write t, t' :: stack when t = t' -> mustNotBeList "written in IR" t &>  stack
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

  | Return, _ ->
      match stack, retType with
      | [], None -> pure' []
      | t' :: stack, Some t -> mustBe t t' &> stack
      | _ -> err "Return type mismatch"

  | Call f, _ -> sanityCheckCall f stack

  | Seq (t, s), _ ->
      sanityCheckInstr t >>= fun stack' ->
      local (fun e -> { e with stack = stack' }) <| sanityCheckInstr s

  | Nop, _ -> pure' stack

  | _ -> err $"Invalid IR instruction:\n{instr}\n{stack}"

and withEmptyStack = local (fun e -> { e with stack = [] }) << sanityCheckInstr

and ensureEmptyStack inLoop label instr =
  withEmptyStack instr
  |> local (fun e -> { e with inLoop = inLoop })
  >>= function
    | [] -> pure' ()
    | _ -> err $"{label} resulted in non-empty stack"

let sanityCheckFn { name = name; args = args; retType = retType } (vars, instr) =
  ensureEmptyStack false name instr
  |> local (fun e -> { e with vars = vars; args = List.map snd args; retType = retType })
  &> (vars, instr)

let sanityCheck p =
  traverseFns sanityCheckFn p
  |> runTC
    { fns = Map.map (const' fst) p.fns
      vars = []
      args = []
      retType = None
      inLoop = false
      stack = []
    }
  |> Result.mapError ((+) "IR sanityCheck error: ")
