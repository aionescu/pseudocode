module Language.Pseudocode.Codegen

open System
open System.Collections.Generic
open System.IO

open FParsec
open Language.Pseudocode.AST

let exit code =
  Environment.Exit code
  Unchecked.defaultof<_>

let btwn s1 s2 s = s1 + s + s2
let paren = btwn "(" ")"
let sqbr = btwn "[" "]"

let compileLit = function
  | NumLit l -> string l
  | BoolLit l -> (string l).ToLower()
  | StringLit l -> "\"" + l + "\""

let rec compileLValue = function
  | Ident i -> i
  | Subscript (i, e) -> i + sqbr (compileExpr e)

and compileArgList es = String.Join(", ", Seq.map compileExpr es)

and compileFuncCall (FuncCall (i, es)) = i + paren (compileArgList es)

and compileTn = function
  | Int -> "int"
  | Float -> "float"
  | Bool -> "bool"
  | String -> "string"

and compileExpr = function
  | Lit l -> compileLit l
  | LValue l -> compileLValue l
  | NewArray (tn, e) -> "new " + compileTn tn + sqbr (compileExpr e)
  | FuncCallExpr fc -> compileFuncCall fc
  | UnOp (op, e) ->  op + paren (compileExpr e)
  | BinOp (e1, op, e2) -> compileExpr e1 + " " + op + " " + compileExpr e2
  | InlineCSharpExpr s -> s.Trim()

let errorUndeclaredVar v =
  printfn "Eroare: Variabila \"%s\" nu a fost declarata." v
  exit 1

let mutable vars = new HashSet<string>()
let mutable importedFiles = new HashSet<string>()

let readString v = v + " = Console.ReadLine();";
let readInt v = v + " = int.Parse(Console.ReadLine());";
let readFloat v = v + " = float.Parse(Console.ReadLine());";
let readBool v = v + " = (int.Parse(Console.ReadLine()) == 0 ? false : true);";

let readType v = function
  | Int -> readInt v
  | Float -> readFloat v
  | Bool -> readBool v
  | String -> readString v

let rec compileAssignment l e =
  match l with
  | Ident i ->
      if vars.Contains(i) then
        i + " = " + compileExpr e + ";"
      else
        vars.Add(i) |> ignore
        "var " + i + " = " + compileExpr e + ";"
  | Subscript (i, s) ->
      if vars.Contains(i) then
        compileLValue l + " = " + compileExpr e + ";"
      else
        errorUndeclaredVar i

and compileRead v t =
  match v with
  | Ident i ->
      match vars.Contains(i) with
      | true -> readType i t
      | false ->
          vars.Add(i) |> ignore
          (compileTn t) + " " + readType i t
  | Subscript (i, e) ->
      match vars.Contains(i) with
      | true -> readType (compileLValue v) t
      | false -> errorUndeclaredVar i (compileExpr e)

and compileFor l e1 e2 e3 =
  let e3 =
    match e3 with
    | None -> "1"
    | Some e3 -> compileExpr e3

  let op = if e3.[0] = '-' then " >= " else " <= "
  let before = if vars.Contains(l) then "" else "var "

  "for (" + before + l + " = " + compileExpr e1 + "; " + l + op + compileExpr e2 + "; " + l + " = " + l + " + " + paren e3 + ") {"

and compileArg (name, type') = compileTn type' + " " + name

and compileArgs = String.concat "," << List.map compileArg

and compileSubalgorithm name args ret =
  let ret =
    match ret with 
    | None -> "void"
    | Some ret -> compileTn ret

  ret + " " + name + "(" + compileArgs args + ") {"

and compileImport (file: string) s =
  let enumerationOptions = EnumerationOptions()
  enumerationOptions.MatchCasing <- MatchCasing.CaseInsensitive

  let files = Directory.GetFiles(Path.GetDirectoryName(Path.GetFullPath(file)), s + ".pseudo", enumerationOptions)

  if files.Length = 0 then
    printfn "Eroare: Modulul '%s' nu a fost gasit." s
    exit 1
  else if files.Length > 1 then
    printfn "Eroare: Mai multe fisiere care pot contine modulul '%s' au fost gasite." s
    exit 1
  else if importedFiles.Contains(s) then
    ""
  else
    importedFiles.Add(s) |> ignore

    let code = File.ReadAllText files.[0]
    let ast = run Parser.programEof code

    match ast with
    | Success (code, _, _) -> compileProgram false files.[0] code
    | _ ->
        printfn "Eroare: Sintaxa incorecta."
        exit 1

and compileStmt file = function
  | FuncCallStatement fc -> compileFuncCall fc + ";"
  | Assignment (l, e) -> compileAssignment l e
  | Read (v, tn) -> compileRead v tn
  | Write es -> "Console.WriteLine(String.Join(\" \", " + compileArgList es + "));"
  | If e -> "if (" + compileExpr e + ") {"
  | ElseIf e -> "} else if (" + compileExpr e + ") {"
  | Else -> "} else {"
  | While e -> "while (" + compileExpr e + ") {"
  | DoWhile e -> "} while (" + compileExpr e + ");"
  | Until e -> "} while (!(" + compileExpr e + "));"
  | Do -> "do {"
  | For (l, e1, e2, e3) -> compileFor l e1 e2 e3
  | Break -> "break;"
  | Continue -> "continue;"
  | End -> "}"
  | Empty -> ""
  | InlineCSharpStatement s -> s.Trim()
  | Subalgorithm (name, args, ret) -> compileSubalgorithm name args ret
  | Return None -> "return;"
  | Return (Some e) -> "return " + compileExpr e + ";"
  | Import s -> compileImport file s

and compileProgram openSystem file (Program stmts) =
  if openSystem then "using System;\n" else ""
  + String.Join("\n", Seq.map (compileStmt file) stmts)