module Main

open System
open System.IO

open Utils
open Compiler.Frontend.Parser
open Compiler.Frontend.CheckDefs
open Compiler.Frontend.TypeCheck
open Compiler.Frontend.FlowAnalysis
open Compiler.Midend.Lowering
open Compiler.Midend.TypeCheckIR
open Compiler.Backend.Codegen

let getInput = function
  | [|"-"|] -> Ok ("<stdin>", Console.In.ReadToEnd())
  | [|path|] ->
      try Ok (path, File.ReadAllText path)
      with e -> Error $"IO error: {e.Message}"
  | _ -> Error "Error: Expecting exactly 1 command-line argument"

let runCompiler args =
  getInput args
  |> Result.bind (parse >=> checkDefs >=> typeCheck >=> flowAnalysis)
  |> Result.bind (lower >> typeCheckIR)
  |> Result.map (compileProgram >> runCompiledProgram)

[<EntryPoint>]
let main argv =
  match runCompiler argv with
  | Error e -> printfn $"{e}"; 1
  | Ok () -> 0
