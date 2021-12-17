module Main

open System.IO

open Utils.Misc
open Utils.Monad.Result
open Frontend.Parser
open Frontend.TypeChecker
open Midend.Renamer
open Midend.Simplifier
open Midend.CoreTypeChecker
open Backend.Codegen

let getInput = function
  | [|path|] ->
      try Ok (path, File.ReadAllText path)
      with e -> Error e.Message
  | _ -> Error "Expecting exactly 1 command-line argument"

let runCompiler args =
  getInput args
  >>= parse
  >>= typeCheck
  <&> rename
  <&> simplify
  >>= typeCheckCore
  <&> compileAndRun

[<EntryPoint>]
let main argv =
  match runCompiler argv with
  | Error e -> printfn $"{e}"; 1
  | Ok () -> 0
