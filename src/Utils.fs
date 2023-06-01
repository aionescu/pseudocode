module Utils

let curry f a b = f (a, b)
let uncurry f (a, b) = f a b

let curry3 f a b c = f (a, b, c)
let curry4 f a b c d = f (a, b, c, d)
let curry5 f a b c d e = f (a, b, c, d, e)

let const' a _ = a
let flip f a b = f b a

let pair a b = (a, b)

let duplicatesBy f l =
  List.groupBy f l
  |> List.filter (fun (_, l) -> List.length l > 1)
  |> List.map fst
