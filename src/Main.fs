module Main

open System.IO

open Utils.Function
open Utils.Monad.Result
module P = Utils.Monad.Parser
open Language.Pseudocode.Parser
open Language.Pseudocode.TypeChecker

let getInput = function
  | [|path|] -> Ok (path, File.ReadAllText path)
  | _ -> Error "Expecting exactly 1 command-line argument"

[<EntryPoint>]
let main argv =
  let result =
    getInput argv
    >>= uncurry (P.parse program)
    >>= typeCheckProgram

  match result with
  | Error e -> printfn $"Error: {e}"; 1
  | Ok stmts -> List.iter (printfn "%A") stmts; 0
