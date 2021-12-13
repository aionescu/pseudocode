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
open Midend.Simplifier
open Midend.CoreTypeChecker
open Backend.CoreEval

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
    >>= fun (max, stmts) ->
      let core = simplifyProgram max stmts
      let vars = varList max
      typeCheckCore vars core
      <&> evalCore vars

  match result with
  | Error e -> printfn $"Error: {e}"; 1
  | Ok () -> 0
