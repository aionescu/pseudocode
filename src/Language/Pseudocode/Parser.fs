module Language.Pseudocode.Parser

open System
open FParsec
open FParsec.Pipes

open Language.Pseudocode.AST

type Parser<'a> = Parser<'a, unit>

let ws = skipMany %[' '; '\t'] .>> opt (%"//" .>> skipRestOfLine false)

let inline (~%%) a = %a .>> ws

let ws1 = skipMany1 %[' '; '\t']
let lineSep = many1 %%[%';'; newline]

let s str = pstringCI str
let ss str = s str .>> ws

let s2 str1 str2 = ss str1 .>> s str2
let ss2 str1 str2 = ss str1 .>> ss str2

let flat4 (((a, b), c), d) = (a, b, c, d)

let bool: Parser<_> = %%[s"fals" >>% BoolLit false; s"adevarat" >>% BoolLit true]

let num: Parser<_> = pfloat .>> ws |>> NumLit

let string: Parser<_> =
  let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')

  let unescape = function
    | 'n' -> '\n'
    | 'r' -> '\r'
    | 't' -> '\t'
    | c -> c

  let escapedChar = %'\\' >>. (anyOf "\\nrt\"" |>> unescape)

  between %'"' %'"' (manyChars %[normalChar; escapedChar]) .>> ws
  |>> StringLit

let inlineCSharp: Parser<_> = %%"{#" >>. manyCharsTill anyChar %%"#}"
let inlineCSharpExpr = inlineCSharp |>> InlineCSharpExpr
let inlineCSharpType = inlineCSharp |>> InlineCSharpType
let inlineCSharpStatement = inlineCSharp |>> InlineCSharpStatement

let lit = %[bool; num; string] |>> Lit

let reserved =
  [ "adevarat"; "fals"; "intreg"; "real"; "logic"; "text"; "si"; "sau"
    "citeste"; "scrie"; "cat"; "timp"; "repeta"; "pana"; "cand"
    "executa"; "pentru"; "daca"; "altfel"; "atunci"; "sfarsit"
    "iesi"; "continua"; "subalgoritm"; "returneaza"; "importa"
  ]

let eqCi s1 s2 =String.Equals(s1, s2, StringComparison.OrdinalIgnoreCase)

let primTypeRaw =
  %%[ ss"intreg" >>% Int
      ss"real" >>% Float
      ss"logic" >>% Bool
      ss"text" >>% String
  ]

let primType = primTypeRaw |>> Prim 

let arrayType = primTypeRaw .>> %%'[' .>> %%']' |>> Array

let typeName = %[attempt inlineCSharpType; attempt arrayType; primType]

let expr, exprImpl = createParserForwardedToRef<Expr, unit> ()

let newArray = primType .>>. between %%'[' %%']' expr |>> NewArray

let identRaw: Parser<_> =
  let fstChar c = c = '_' || isLetter c
  let sndChar c = fstChar c || isDigit c

  let ident = many1Satisfy2 fstChar sndChar .>> ws
  ident >>= fun s ->
    if List.exists (eqCi s) reserved || s.StartsWith("__") then
      fail "Identificator rezervat"
    else
      preturn (s.ToLower())

let ident = identRaw |>> Ident

let subscript =
  identRaw .>>. between %%'[' %%']' expr |>> Subscript

let lvalueRaw = %[attempt subscript; ident]
let lvalue = lvalueRaw |>> LValue

let argList = sepBy expr %%','

let funcCallRaw = identRaw .>>. between %%'(' %%')' argList |>> FuncCall

let funcCall = funcCallRaw |>> FuncCallExpr

let parensExpr = between %%'(' %%')' expr

let opp = OperatorPrecedenceParser<Expr,unit,unit> ()

exprImpl := opp.ExpressionParser
opp.TermParser <- %[attempt funcCall; attempt newArray; attempt lvalue; attempt inlineCSharpExpr; lit; parensExpr]

opp.AddOperator(PrefixOperator("-", ws, 1, true, fun x -> UnOp ("-", x)))
opp.AddOperator(PrefixOperator("!", ws, 1, true, fun x -> UnOp ("!", x)))

let ops =
  [ "+"
    "-"
    "*"
    "/"
    "%"
    "^"
    "<"
    "<="
    ">="
    ">"
  ]

for op in ops do
  opp.AddOperator(InfixOperator(op, ws, 1, Associativity.Left, fun x y -> BinOp (x, op, y)))

opp.AddOperator(InfixOperator("++", ws, 1, Associativity.Left, fun x y -> BinOp (x, "+", y)))
opp.AddOperator(InfixOperator("si", ws, 1, Associativity.Left, fun x y -> BinOp (x, "&&", y)))
opp.AddOperator(InfixOperator("sau", ws, 1, Associativity.Left, fun x y -> BinOp (x, "||", y)))
opp.AddOperator(InfixOperator("=", ws, 1, Associativity.Left, fun x y -> BinOp (x, "==", y)))
opp.AddOperator(InfixOperator("<>", ws, 1, Associativity.Left, fun x y -> BinOp (x, "!=", y)))

let funcCallStatement = funcCallRaw |>> FuncCallStatement

let assignment = lvalueRaw .>> %%"<-" .>>. expr |>> Assignment

let read = s"citeste" >>. ws1 >>. lvalueRaw .>>. between %%'(' %%')' typeName |>> Read

let write = s"scrie" >>. ws1 >>. argList |>> Write

let if' = s"daca" >>. ws1 >>. expr .>> %%"atunci" |>> If

let sf = %%[s"sfarsit"; s"sf"]

let end' = sf .>> opt %%[ss"daca"; ss2"cat""timp"; ss"pentru"; ss"subalgoritm"] >>% End

let elseIf = s2"altfel""daca" >>. ws1 >>. expr .>> ss"atunci" |>> ElseIf

let else' = ss"altfel" >>% Else

let while' = s2"cat""timp" >>. ws1 >>. expr .>> ss"executa" |>> While

let doWhile = s2"cat""timp" >>. ws1 >>. expr |>> DoWhile

let until = s2"pana""cand" >>. ws1 >>. expr |>> Until

let execute = ss"executa" >>% Do

let repeat = ss"repeta" >>% Do

let return' = ss"returneaza" >>. opt expr |>> Return

let arg = identRaw .>> %%':' .>>. typeName

let subalgorithm =
  ss"subalgoritm" >>. identRaw .>>. between %%'(' %%')' (sepBy arg %%',') .>>. opt (%%':' >>. typeName)
  |>> fun ((a, b), c) -> Subalgorithm (a, b, c)

let import = ss"importa" >>. identRaw |>> Import

let for' =
  s"pentru" >>. ws1 >>. identRaw .>> %%"<-" .>>. expr .>> %%',' .>>. expr .>>. opt (%%',' >>. expr) .>> ss"executa" |>> (flat4 >> For)

let empty = preturn Empty

let break' = ss"iesi" >>% Break
let continue' = ss"continua" >>% Continue

let statement' =
  [ inlineCSharpStatement
    import; subalgorithm; return'
    for'; repeat; execute; until
    while'; doWhile; elseIf; else'
    if'; write; read; assignment
    funcCallStatement; break'
    continue'; end'; empty
  ]

let statement = %(List.map attempt statement')

let program = sepBy statement lineSep |>> Program

let programEof = program .>> eof
