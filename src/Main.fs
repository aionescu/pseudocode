﻿module Main

open System
open System.IO

open Utils
open Compiler.Frontend.Parser
open Compiler.Frontend.TypeChecker
open Compiler.Midend.Renamer
open Compiler.Midend.Simplifier
open Compiler.Midend.SanityCheck
open Compiler.Backend.Codegen

let getInput = function
  | [|"-"|] -> Ok ("<stdin>", Console.In.ReadToEnd())
  | [|path|] ->
      try Ok (path, File.ReadAllText path)
      with e -> Error $"IO error: {e.Message}"
  | _ -> Error "Error: Expecting exactly 1 command-line argument"

let runCompiler args =
  getInput args
  |> Result.bind (parse >=> typeCheck)
  |> Result.map (rename >> simplify)
  |> Result.bind sanityCheck
  |> Result.map compileAndRun

[<EntryPoint>]
let main argv =
  match runCompiler argv with
  | Error e -> printfn $"{e}"; 1
  | Ok () -> 0
