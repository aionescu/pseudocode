module Compiler.Frontend.FlowAnalysis

open Utils
open Monad.TC
open Compiler.Frontend.AST

let panic () = failwith "Panic in FlowAnalysis"

let rec alwaysReturns = function
  | Return _ -> true
  | Let (_, _, _, s) -> alwaysReturns s
  | Seq (a, b) -> alwaysReturns a || alwaysReturns b
  | If (_, t, f) -> alwaysReturns t && alwaysReturns f
  | DoWhile (s, _) -> alwaysReturns s
  | _ -> false

let flowAnalysisFn fnSig body =
  if alwaysReturns body || fnSig.retType = None then
    pure' body
  else
    err $"Not all code paths return a value in function \"{fnSig.name}\""

let flowAnalysis p =
  traverseFns flowAnalysisFn p
  |> runTC ()
  |> Result.mapError ((+) "Control flow error: ")
