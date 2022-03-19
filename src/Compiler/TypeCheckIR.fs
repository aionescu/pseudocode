module Compiler.TypeCheckIR

open Utils.Misc
open Utils.TC
open Compiler.AST
open Compiler.TypeCheck
open Compiler.IR

type TypeCheckIREnv =
  { fns: Map<Id, FnSig>
    vars: Map<Id, Type>
    retType: Type option
    inLoop: bool
    stack: Type list
  }

let lookupVar v =
  asks (fun e -> Map.tryFind v e.vars)
  >>= (explain $"Variable \"{v}\" not allocated" >> lift)

let typeCheckCall f stack =
  asks (fun e -> e.fns[f]) >>= fun { args = args; retType = retType } ->
  let argc = List.length args
  let args = List.map snd args
  let stackArgs = List.rev <| List.take argc stack

  if stackArgs <> args then
    err $"Invalid args in Call \"${f}\": Expected {args}, found {stackArgs}"
  else
    pure' <| Option.toList retType @ List.skip argc stack

let rec typeCheckInstr instr =
  ask >>= fun { retType = retType; inLoop = inLoop; stack = stack } ->
  match instr, stack with
  | PushBool _, _ -> pure' (Bool :: stack)
  | PushInt _, _ -> pure' (Int :: stack)
  | PushFloat _, _ -> pure' (Float :: stack)
  | PushString _, _ -> pure' (String :: stack)
  | NewList t, stack -> pure' (List t :: stack)

  | Let (i, t, e, s), [] ->
      typeCheckInstr e >>= function
        | [t'] ->
            mustBe t t'
            *> local
              (fun e -> { e with vars = Map.add i t e.vars })
              (typeCheckInstr s)
        | _ -> err "Invalid Let initializer"

  | LoadVar v, _ -> lookupVar v <&> fun t -> t :: stack
  | SetVar v, t :: stack ->
      lookupVar v >>= fun v ->
      mustBe v t &> stack

  | Dup, t :: stack -> pure' (t :: t :: stack)

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

  | While (c, s), [] ->
      typeCheckInstr c >>= function
        | [Bool] -> ensureEmptyStack true "While body" s &> stack
        | _ -> err "Invalid While condition"

  | DoWhile (s, c), [] ->
      typeCheckInstr c >>= function
        | [Bool] -> ensureEmptyStack true "While body" s &> stack
        | _ -> err "Invalid DoWhile condition"

  | For (i, a, _, b, s), [] ->
      pair <!> typeCheckInstr a <*> typeCheckInstr b >>= function
        | [Int], [Int] ->
            local
              (fun e -> { e with vars = Map.add i Int e.vars })
              (ensureEmptyStack true "For body" s)
            &> stack
        | [Int], b -> err $"Invalid For bound: Expected [Int], found {b}"
        | a, _ -> err $"Invalid For initializer: Expected [Int], found {a}"

  | (Break | Continue), [] when inLoop -> pure' []

  | Return, _ ->
      match stack, retType with
      | [], None -> pure' []
      | t' :: stack, Some t -> mustBe t t' &> stack
      | _ -> err "Return type mismatch"

  | Call f, _ -> typeCheckCall f stack

  | Seq (t, s), _ ->
      typeCheckInstr t >>= fun stack' ->
      local (fun e -> { e with stack = stack' }) <| typeCheckInstr s

  | Nop, _ -> pure' stack

  | _ -> err $"Invalid IR instruction:\n{instr}\n{stack}"

and ensureEmptyStack inLoop label instr =
  typeCheckInstr instr
  |> local (fun e -> { e with inLoop = inLoop; stack = [] })
  >>= function
    | [] -> pure' ()
    | _ -> err $"{label} resulted in non-empty stack"

let typeCheckFn { name = name; args = args; retType = retType } instr =
  ensureEmptyStack false name instr
  |> local (fun e -> { e with vars = Map.ofList args; retType = retType })
  |> mapErr ((+) $"In function \"{name}\": ")
  &> instr

let typeCheckIR p =
  traverseFns typeCheckFn p
  |> mapErr ((+) "IR type error: ")
  |> runTC
    { fns = mapVals fst p.fns
      vars = Map.empty
      retType = None
      inLoop = false
      stack = []
    }
