module Utils.Misc

// Function

let const' a _ = a
let flip f a b = f b a

let curry f a b = f (a, b)
let uncurry f (a, b) = f a b

let curry3 f a b c = f (a, b, c)
let curry4 f a b c d = f (a, b, c, d)
let curry5 f a b c d e = f (a, b, c, d, e)

let (>..) f g a b = g <| f a b

// Pair

let pair a b = (a, b)

// List

let cons x xs = x :: xs

let foldr f z xs = List.foldBack f xs z

let duplicatesBy f l =
  List.groupBy f l
  |> List.filter (fun (_, l) -> List.length l > 1)
  |> List.map fst

// Map

let mapVals f = Map.map (const' f)
