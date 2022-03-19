module Main

open System
open System.IO

open Utils.Misc
open Compiler.Parser
open Compiler.CheckDefs
open Compiler.TypeCheck
open Compiler.FlowAnalysis
open Compiler.Lowering
open Compiler.TypeCheckIR
open Compiler.Codegen

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
