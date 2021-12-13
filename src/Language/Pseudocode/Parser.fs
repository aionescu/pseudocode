module Language.Pseudocode.Parser

open System
open FParsec

open Utils.Function
open Utils.Monad.Parser
open Language.Pseudocode.Syntax

// Misc Parsers

let singleLineComment = pstring "--" *> skipRestOfLine true
let multiLineComment = pstring "{-" *> skipManyTill anyChar (pstring "-}")
let endLine =
  let r = skipChar '\r'
  let n = skipChar '\n'
  choice [attempt <| r *> n; r; n]

let ws1 = skipMany1 (choice [skipChar ' '; skipChar '\t'; multiLineComment])
let ws = ws1 <|>% ()

let wsMulti = skipMany (choice [singleLineComment; endLine; ws1])
let stmtSep = choice [skipChar ';'; singleLineComment; endLine] *> wsMulti

let comma = pchar ',' <* ws
let equals = pchar '=' <* ws
let colon = pchar ':' <* ws

let choice' ps = choice <| List.map attempt ps

// Exprs

let boolLit =
  choice [
    stringReturn "False" (U <| BoolLit false)
    stringReturn "True" (U <| BoolLit true)
  ]

let numberFormat =
  NumberLiteralOptions.AllowMinusSign
  ||| NumberLiteralOptions.AllowFraction
  ||| NumberLiteralOptions.AllowExponent

let numLit =
  numberLiteral numberFormat "numeric literal" <&> fun nl ->
    if nl.IsInteger then
      U << IntLit <| int nl.String
    else
      U << RealLit <| float nl.String

let textLit: Parser<_> =
  let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')

  let unescape = function
    | 'n' -> '\n'
    | 'r' -> '\r'
    | 't' -> '\t'
    | c -> c

  let escapedChar = pchar '\\' *> (anyOf "\\nrt\"" <&> unescape)

  between (pchar '"') (pchar '"') (manyChars <| choice [normalChar; escapedChar])
  <&> (TextLit >> U)

let expr, exprRef = createParserForwardedToRef ()

let arrayLit =
  between (pchar '[' <* ws) (pchar ']' <* ws) (sepBy expr comma)
  <&> (ArrayLit >> U)

let reserved =
  [ "let"; "end"; "if"; "then"; "else"; "while"; "do"; "for"; "in"; "to"; "and"; "or"; "not"; "read"; "write"
    "Integer"; "Real"; "Text"; "Boolean";
    "True"; "False"
  ]

let ident =
  let fstChar c = c = '_' || c = '\'' || isLetter c
  let sndChar c = fstChar c || isDigit c

  many1Satisfy2 fstChar sndChar >>= fun s ->
    if List.contains s reserved then
      fail $"Reserved identifier \"{s}\""
    else
      preturn s

let var = ident <&> (Var >> U)

let parensExpr = between (pchar '(' <* ws) (pchar ')' <* ws) expr
let exprSimple = choice' [boolLit; numLit; textLit; arrayLit; var; parensExpr] <* ws

let withSubscript e =
  let subscript = between (pchar '[' <* ws) (pchar ']' <* ws) expr
  let unrollSubscript = List.fold <| curry (Subscript >> U)

  unrollSubscript <!> e <*> many subscript

let opp = OperatorPrecedenceParser ()

exprRef.Value <- opp.ExpressionParser
opp.TermParser <- withSubscript exprSimple

opp.AddOperator(PrefixOperator("-", ws, 1, true, Negate >> U))
opp.AddOperator(PrefixOperator("not", ws, 1, true, Not >> U))

let ops =
  let arith = curry3 Arith
  let comp = curry3 Comp
  let logic = curry3 Logic

  [ ["and", logic And; "or", logic Or], Associativity.Right
    ["==", comp Eq; "!=", comp Neq; "<", comp Lt; "<=", comp Lte; ">", comp Gt; ">=", comp Gte], Associativity.None
    ["+", arith Add; "-", arith Sub], Associativity.Left
    ["*", arith Mul; "/", arith Div; "%", arith Mod], Associativity.Left
    ["^", curry Pow; "<>", curry Append], Associativity.Right
  ]

for prec, (ops, assoc) in List.indexed ops do
  for (opStr, op) in ops do
    opp.AddOperator(InfixOperator(opStr, ws, prec + 1, assoc, op >.. U))

// Types

let primType =
  choice [
    stringReturn "Integer" Int
    stringReturn "Real" Real
    stringReturn "Text" Text
    stringReturn "Boolean" Bool
  ]

let type', typeRef = createParserForwardedToRef ()

let arrayType = between (pchar '[' <* ws) (pchar ']' <* ws) type' |>> Array
typeRef.Value <- choice [arrayType; primType] <* ws

// Stmts

let lvalue = withSubscript (var <* ws)

let let' =
  curry3 Let
  <!> (pstring "let" *> ws *> ident <* ws)
  <*> opt (colon *> type')
  <*> (equals *> expr)

let assign = curry Assign <!> (lvalue <* equals) <*> expr

let read = pstring "read" *> ws *> lvalue <&> Read

let write = pstring "write" *> ws *> sepBy expr comma <&> Write

let stmt, stmtRef = createParserForwardedToRef ()

let stmts = many (stmt <* stmtSep)

let end' = pstring "end"

let if' =
  curry3 If
  <!> (pstring "if" *> ws *> expr)
  <*> (pstring "then" *> stmtSep *> stmts)
  <*> ((pstring "else" *> stmtSep *> stmts) <|>% [] <* end')

let while' =
  curry While
  <!> (pstring "while" *> ws *> expr)
  <*> (pstring "do" *> stmtSep *> stmts <* end')

let for' =
  curry4 For
  <!> (pstring "for" *> ws *> ident <* ws)
  <*> (equals *> expr)
  <*> (pstring "to" *> ws *> expr)
  <*> (pstring "do" *> stmtSep *> stmts <* end')

stmtRef.Value <-
  choice' [for';  while'; if'; write; read; assign; let']

let program: Program Parser = stmts <* eof
