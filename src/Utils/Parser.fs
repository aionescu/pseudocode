module Utils.Parser

open FParsec

type Parser<'a> = Parser<'a, unit>

let (<&>) = (|>>)
let (<!>) f a = a <&> f

let (&>) = (>>%)
let (<&) v a = a &> v

let (<*>) f a = pipe2 f a (<|)

let ( *> ) = (>>.)
let ( <* ) = (.>>)

let (=<<) f a = a >>= f

let parse parser name input =
  match runParserOnString parser () name input with
  | Success (a, _, _) -> Result.Ok a
  | Failure (e, _, _) -> Result.Error e
