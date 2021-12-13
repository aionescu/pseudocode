module Backend.CoreEval

open Frontend.Syntax
open Midend.Core
open Backend.Eval
open System
open Utils.Function

let panic () = failwith "Panic in CoreEval"

let inline arith op =
  match op with
  | Add -> (+)
  | Sub -> (-)
  | Mul -> (*)
  | Div -> (/)
  | Mod -> (%)

let inline comp op =
  match op with
  | Eq -> (=)
  | Neq -> (<>)
  | Lt -> (<)
  | Lte -> (<=)
  | Gt -> (>)
  | Gte -> (>=)

let rec evalInstr (env: _[]) stack instr =
  match instr, stack with
  | PushInt i, _ -> VInt i :: stack
  | PushReal r, _ -> VReal r :: stack
  | PushBool b, _ -> VBool b :: stack
  | PushText t, _ -> VText t :: stack
  | NewArr _, VInt n :: stack -> VArray (Array.create n Unchecked.defaultof<_>) :: stack
  | LoadVar i, _ -> env[i] :: stack
  | SetVar i, v :: stack -> env[i] <- v; stack
  | Dup, v :: stack -> v :: v :: stack
  | LoadIndex, VInt i :: VArray a :: stack -> a[i] :: stack
  | SetIndex, v :: VInt i :: VArray a :: stack -> a[i] <- v; stack

  | Read t, _ ->
      let line = Console.ReadLine()
      let v =
        match t with
        | Int -> VInt <| int line
        | Real -> VReal <| float line
        | Bool -> VBool <| Boolean.Parse(line)
        | Text -> VText line
        | _ -> panic ()

      v :: stack

  | Write t, v :: stack -> Console.Write(showVal(v)); stack
  | WriteLine, _ -> Console.WriteLine(); stack
  | Not, VBool b :: stack -> VBool (not b) :: stack

  | Negate, VInt i :: stack -> VInt -i :: stack
  | Negate, VReal r :: stack -> VReal -r :: stack

  | Append false, VText b :: VText a :: stack -> VText (a + b) :: stack
  | Append true, VArray b :: VArray a :: stack -> VArray (Array.append a b) :: stack

  | Pow, VReal b :: VReal a :: stack -> VReal (a ** b) :: stack

  | Arith op, VInt b :: VInt a :: stack -> VInt (arith op a b) :: stack
  | Arith op, VReal b :: VReal a :: stack -> VReal (arith op a b) :: stack

  | Comp (op, _), b :: a :: stack -> VBool (comp op a b) :: stack

  | If (t, _), VBool true :: stack -> evalInstrs env stack t
  | If (_, e), VBool false :: stack -> evalInstrs env stack e

  | While (c, is) as w, _ ->
      match evalInstrs env stack c with
      | VBool true :: stack -> evalInstrs env stack (is @ [w])
      | VBool false :: stack -> stack
      | _ -> panic ()

  | _ -> panic ()

and evalInstrs env stack = function
  | [] -> stack
  | instr :: instrs ->
      evalInstr env stack instr
      |> flip (evalInstrs env) instrs

let evalCore vars instrs =
  let vars = Array.create (List.length vars) Unchecked.defaultof<_>
  ignore <| evalInstrs vars [] instrs
