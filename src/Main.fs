module Main

open System.IO

open Utils.Function
open Utils.Result
module P = Utils.Parser
open Frontend.Parser
open Frontend.TypeChecker
open Backend.Eval
open Frontend.Syntax
open Midend.Renamer

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
    <&> allocVarsProgram

  match result with
  | Error e -> printfn $"Error: {e}"; 1
  | Ok p -> printfn $"{p}"; 0
