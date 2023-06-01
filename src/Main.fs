module Main

open System
open System.IO

open Language.Pseudocode.Parser
open Language.Pseudocode.TypeChecking
open Language.Pseudocode.Lowering
open Language.Pseudocode.Codegen

let getInput = function
  | [|"-"|] -> Ok ("<stdin>", Console.In.ReadToEnd())
  | [|path|] ->
      try Ok (path, File.ReadAllText path)
      with e -> Error $"IO error: {e.Message}"
  | _ -> Error "Error: Expecting exactly 1 command-line argument"

let runCompiler args =
  getInput args
  |> Result.bind (parse >> Result.bind typeCheck)
  |> Result.map (lower >> compile >> runProgram)

[<EntryPoint>]
let main argv =
  match runCompiler argv with
  | Error e -> printfn $"{e}"; 1
  | Ok () -> 0
