module Language.Pseudocode.Parser

open FParsec

open Utils
open Language.Pseudocode.Syntax

type Parser<'a> = Parser<'a, unit>

let (<!>) f a = a |>> f
let (<*>) f a = pipe2 f a (<|)

// Misc Parsers

let singleLineComm = pstring "--" >>. skipRestOfLine true
let multiLineComm = pstring "{-" >>. skipManyTill anyChar (pstring "-}")

let ws1 = skipMany1 (choice [skipChar ' '; skipChar '\t'; multiLineComm])
let ws = ws1 <|>% ()

let lexeme p = p .>> ws
let symbol s = pstring s .>> ws

let wsMulti = skipMany (choice [singleLineComm; skipNewline; ws1])
let stmtSep = choice [skipChar ';'; singleLineComm; skipNewline; eof] >>. wsMulti

let parens p = between (symbol "(") (symbol ")") p
let sqBr p = between (symbol "[") (symbol "]") p

let comma = symbol ","
let equals = symbol "="
let colon = symbol ":"

// Exprs

let numLit =
  let numFormat = NumberLiteralOptions.AllowMinusSign ||| NumberLiteralOptions.AllowFraction ||| NumberLiteralOptions.AllowExponent
  lexeme (numberLiteral numFormat "numeric literal") |>> fun nl ->
    if nl.IsInteger then
      U << IntLit <| int nl.String
    else
      U << FloatLit <| float nl.String

let stringLit: Parser<_> =
  let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')

  let unescape = function
    | 'n' -> '\n'
    | 'r' -> '\r'
    | 't' -> '\t'
    | c -> c

  let escapedChar = pchar '\\' >>. (anyOf "\\nrt\"" |>> unescape)

  manyChars (normalChar <|> escapedChar)
  |> between (pchar '"') (pchar '"')
  |> lexeme
  |>> (StringLit >> U)

let expr, exprRef = createParserForwardedToRef ()

let listLit = sqBr (sepBy expr comma) |>> (ListLit >> U)

let reserved =
  [ "let"; "end"; "if"; "then"; "else"; "while"; "do"; "for"; "down"; "to"
    "and"; "or"; "not"; "read"; "write"; "length"; "break"; "continue"
    "push"; "pop"; "Bool"; "Int"; "Float"; "String"; "True"; "False"
    "return"; "function"; "program"
  ]

let ident =
  let fstChar c = c = '_' || c = '\'' || isLetter c
  let sndChar c = fstChar c || isDigit c

  lexeme (many1Satisfy2 fstChar sndChar) >>= fun s ->
    if List.contains s reserved then
      fail $"Reserved identifier \"{s}\""
    else
      preturn s

let var = ident |>> (Var >> U)

let fnCall = ident .>>. parens (sepBy expr comma)

let exprSimple =
  choice
    [ symbol "False" >>% U (BoolLit false)
      symbol "True" >>% U (BoolLit true)
      numLit
      stringLit
      listLit
      parens expr
      attempt fnCall |>> (FnCall >> U)
      attempt var
    ]

let withSubscript e = List.fold (curry (Subscript >> U)) <!> e <*> many (sqBr expr)

let ws' = ws >>. notFollowedByL (pchar '-') "minus"

let opp = OperatorPrecedenceParser ()
opp.TermParser <- withSubscript exprSimple

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

let infixPrec = List.length ops + 1

opp.AddOperator(PrefixOperator("-", ws', infixPrec, true, Negate >> U))
opp.AddOperator(PrefixOperator("not", ws', infixPrec, true, Not >> U))
opp.AddOperator(PrefixOperator("read", ws', infixPrec, true, Read >> U))
opp.AddOperator(PrefixOperator("length", ws', infixPrec, true, Length >> U))

for prec, (ops, assoc) in List.indexed ops do
  for (opStr, op) in ops do
    opp.AddOperator(InfixOperator(opStr, ws', prec + 1, assoc, fun a b -> U <| op a b))

exprRef.Value <- opp.ExpressionParser

// Types

let ty, tyRef = createParserForwardedToRef ()
tyRef.Value <-
  choice [
    symbol "Bool" >>% Bool
    symbol "Int" >>% Int
    symbol "Float" >>% Float
    symbol "String" >>% String
  ] <|> (sqBr ty |>> List)

// Stmts

let lvalue = withSubscript (attempt var)

let stmt, stmtRef = createParserForwardedToRef ()

let let' =
  curry4 Let
  <!> (symbol "let" >>. ident)
  <*> opt (colon >>. ty)
  <*> (equals >>. expr)
  <*> (stmtSep >>. stmt)

let assign = curry Assign <!> (lvalue .>> equals) <*> (expr .>> stmtSep)

let push =
  curry Push
  <!> (symbol "push" >>. lvalue)
  <*> many1 (comma >>. expr) .>> stmtSep

let pop = symbol "pop" >>. lvalue .>> stmtSep |>> Pop

let write = symbol "write" >>. sepBy1 expr comma .>> stmtSep |>> Write

let break' = symbol "break" >>. stmtSep >>% Break
let continue' = symbol "continue" >>. stmtSep >>% Continue

let return' = symbol "return" >>. opt expr .>> stmtSep |>> Return
let fnCallStmt = fnCall .>> stmtSep |>> FnCallStmt

let end' = symbol "end" >>. stmtSep

let if' =
  let if' = symbol "if"
  let then' = symbol "then"
  let else' = symbol "else"
  let elseIf = (else' >>. if' >>. expr) .>>. (then' >>. stmtSep >>. stmt)

  let rec unrollIf c t es e = If (c, t, List.foldBack (uncurry (curry3 If)) es e)

  unrollIf
  <!> (if' >>. expr)
  <*> (then' >>. stmtSep >>. stmt)
  <*> many (attempt elseIf)
  <*> ((else' >>. stmtSep >>. stmt) <|>% Nop .>> end')

let while' =
  curry While
  <!> (symbol "while" >>. expr)
  <*> (symbol "do" >>. stmtSep >>. stmt .>> end')

let doWhile =
  curry DoWhile
  <!> (symbol "do" >>. stmtSep >>. stmt)
  <*> (symbol "while" >>. expr .>> stmtSep)

let for' =
  curry5 For
  <!> (symbol "for" >>. ident)
  <*> (equals >>. expr)
  <*> ((symbol "down" >>% true) <|>% false)
  <*> (symbol "to" >>. expr)
  <*> (symbol "do" >>. stmtSep >>. stmt .>> end')

let stmtSimple =
  choice
    [ let'; attempt while'; doWhile; for'; if'; write
      push; pop; break'; continue'; return'
      attempt assign; attempt fnCallStmt
    ]

stmtRef.Value <-
  many stmtSimple
  |>> fun ss -> List.foldBack (curry Seq) ss Nop

let fnSig =
  let mkFnSig n a r = { name = n; args = a; retTy = r }
  let arg = ident .>> colon .>>. ty
  let args = parens (sepBy arg comma)
  let ret = opt (colon >>. ty)

  (symbol "program" >>% mkFnSig "program" [] None)
  <|> (symbol "function" >>. (mkFnSig <!> ident <*> args <*> ret))

let function' = fnSig .>> stmtSep .>>. stmt .>> end'

let program = wsMulti >>. many function' .>> eof

let mkProgram fns =
  let duplicates = duplicatesBy (fun ({ name = n }, _) -> n) fns

  match duplicates with
  | name :: _ -> Result.Error $"Duplicate definition of function \"{name}\""
  | [] -> Result.Ok { fns = Map.ofList <| List.map (fun ({ name = name }, _ as fn) -> name, fn) fns }

let parse (name, code) =
  match runParserOnString program () name code with
  | Failure (e, _, _) -> Result.Error ("Parser error:\n" + e)
  | Success (fns, _, _) -> mkProgram fns
