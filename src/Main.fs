module Main

open System.IO

open Utils.Misc
open Utils.Monad.Result
module P = Utils.Monad.Parser
open Frontend.Parser
open Frontend.TypeChecker
open Frontend.Syntax
open Midend.Renamer
open Midend.Simplifier
open Midend.CoreTypeChecker
open Backend.Codegen

let getInput = function
  | [|path|] ->
      try Ok (path, File.ReadAllText path)
      with e -> Error e.Message
  | _ -> Error "Expecting exactly 1 command-line argument"

[<EntryPoint>]
let main argv =
  let result =
    getInput argv
    >>= uncurry (P.parse program)
    >>= typeCheckProgram
    <&> renameProgram
    <&> second simplifyProgram
    >>= (fun (vars, instrs) -> pair vars <!> typeCheckCore vars instrs)
    <&> uncurry compileAndRun

  match result with
  | Error e -> printfn $"Error: {e}"; 1
  | Ok () -> 0
