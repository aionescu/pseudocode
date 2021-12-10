module Main

open System.IO

open Utils.Function
open Utils.Monad.Result
module P = Utils.Monad.Parser
open Language.Pseudocode.Parser
open Language.Pseudocode.TypeChecker
open Language.Pseudocode.Eval

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
    <&> evalProgram

  match result with
  | Error e -> printfn $"Error: {e}"; 1
  | Ok () -> 0
