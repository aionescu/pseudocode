module Language.Pseudocode.Main

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

  let code = File.ReadAllText file
  
  let parseResult = run programEof code

  if debug then
    printfn "[Debug.ParserResult]\n%A\n" parseResult

  let success, code =
    match run programEof code with
    | Success (code, _, _) -> true, compileProgram code
    | _ -> false, "Eroare: Sintaxa incorecta."

  if success then
    let script = CSharpScript.Create(code)
    let diagnostics = script.Compile()

    if debug then
      printfn "[Debug.CodegenResult]\n%s\n" code

      printfn "[Debug.CSharpErrors]\nEmpty? %b\n%A\n" diagnostics.IsEmpty diagnostics

    if diagnostics.IsEmpty then
      script.RunAsync().Wait()
    else
      printfn "Eroare: Sintaxa incorecta sau utilizare variabile de tipuri incompatibile."
  else // Parser failed
    printfn "%s" code

  0