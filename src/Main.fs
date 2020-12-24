module Main

open System.IO
open FParsec
open Microsoft.CodeAnalysis.CSharp.Scripting

open Language.Pseudocode.Parser
open Language.Pseudocode.Codegen

[<EntryPoint>]
let main argv =
  let debug, file =
    if argv.Length = 2 && (argv.[0] = "-d" || argv.[0] = "--debug") then
      true, argv.[1]
    elif argv.Length < 1 then
      printfn "Eroare: Niciun fisier sursa nu a fost specificat."
      exit 1
    elif argv.Length > 1 then
      printfn "Eroare: Prea multe fisiere au fost specificate."
      exit 1
    else
      false, argv.[0]

  let code =
    try File.ReadAllText file
    with e ->
      printfn "Eroare: %s" e.Message
      exit 1

  let parseResult = run programEof code

  if debug then
    printfn "[Parse Result]\n%A\n" parseResult

  let success, code =
    match parseResult with
    | Success (code, _, _) -> true, compileProgram true file code
    | _ -> false, "Eroare: Sintaxa incorecta."

  if success then
    let script = CSharpScript.Create(code)
    let diagnostics = script.Compile()

    if debug then
      let codeLines = code.Split("\n")
      let maxLineDigitCount = codeLines.Length.ToString().Length

      let codeWithLines =
        codeLines
        |> Seq.append [""]
        |> Seq.mapi (sprintf "%*d: %s" maxLineDigitCount)
        |> Seq.skip 1
        |> String.concat "\n"

      printfn "[Codegen Result]\n%s\n" codeWithLines

      printfn "[C# Errors]\n%A\n" <| Seq.toList diagnostics

    if diagnostics.IsEmpty then
      script.RunAsync().Wait()
    else
      printfn "Eroare: Sintaxa incorecta sau utilizare variabile de tipuri incompatibile."
  else // Parser failed
    printfn "%s" code

  0
