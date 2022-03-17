module Compiler.Frontend.FlowAnalysis

open Utils
open Monad.TC
open Compiler.Frontend.Syntax

let panic () = failwith "Panic in FlowAnalysis"

let rec alwaysReturns = function
  | Return _ -> true
  | Let (_, _, _, s) -> alwaysReturns s
  | Seq (a, b) -> alwaysReturns a || alwaysReturns b
  | If (_, t, f) -> alwaysReturns t && alwaysReturns f
  | DoWhile (s, _) -> alwaysReturns s
  | _ -> false

let flowAnalysisFn fnSig body =
  match alwaysReturns body, fnSig.retType with
  | true, _ -> pure' body
  | false, None -> pure' <| Seq (body, Return None)
  | _ -> err $"Not all code paths return a value in function \"{fnSig.name}\""

let flowAnalysis p =
  traverseFns flowAnalysisFn p
  |> runTC ()
  |> Result.mapError ((+) "Control flow error: ")
