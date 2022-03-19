module Compiler.CheckDefs

open Utils.TC
open Compiler.AST

let duplicatesBy f l =
  List.groupBy f l
  |> List.filter (fun (_, l) -> List.length l > 1)
  |> List.map (fun (name, _) -> name)

let checkDuplicateFns fns =
  let duplicates = duplicatesBy (fun ({ name = n }, _) -> n) fns

  match duplicates with
  | name :: _ -> err $"Duplicate definition for function \"{name}\""
  | [] -> pure' { fns = Map.ofList <| List.map (fun ({ name = name }, _ as fn) -> name, fn) fns }

let checkProgramSig p =
  match Map.tryFind "program" p.fns with
  | None -> err "Missing program definition"
  | Some ({ args = []; retType = None }, _) -> pure' p
  | _ -> err "Invalid signature for \"program\" function"

let checkDuplicateArgs fnSig body =
  match duplicatesBy fst fnSig.args with
  | [] -> pure' body
  | arg :: _ -> err $"Duplicate definition for argument \"{arg}\" in function \"{fnSig.name}\""

let checkArgsShadowing fns { name = name; args = args } body =
  List.map fst args
  |> List.filter (fun name -> Map.containsKey name fns)
  |> function
    | [] -> pure' body
    | arg :: _ -> err $"Argument \"{arg}\" in function \"{name}\" shadows a function with the same name"

let checkDefs fns =
  checkDuplicateFns fns
  >>= checkProgramSig
  >>= traverseFns checkDuplicateArgs
  >>= fun p -> traverseFns (checkArgsShadowing p.fns) p
  |> mapErr ((+) "Error: ")
  |> runTC ()
