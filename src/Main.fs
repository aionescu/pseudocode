module Main

open System.IO

open Utils
open Compiler.Frontend.Parser
open Compiler.Frontend.TypeChecker
open Compiler.Midend.Renamer
open Compiler.Midend.Simplifier
open Compiler.Backend.Codegen

let getInput = function
  | [|path|] ->
      try Ok (path, File.ReadAllText path)
      with e -> Error e.Message
  | _ -> Error "Expecting exactly 1 command-line argument"

let runCompiler args =
  getInput args
  |> Result.bind parse
  |> Result.bind typeCheck
  |> Result.map (rename >> simplify >> compileAndRun)

[<EntryPoint>]
let main argv =
  match runCompiler argv with
  | Error e -> printfn $"{e}"; 1
  | Ok () -> 0
