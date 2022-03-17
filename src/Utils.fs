module Utils

// Function

let flip f a b = f b a
let const' a _ = a

let curry f a b = f (a, b)
let uncurry f (a, b) = f a b

let curry3 f a b c = f (a, b, c)
let curry4 f a b c d = f (a, b, c, d)
let curry5 f a b c d e = f (a, b, c, d, e)

let (>..) f g a b = g <| f a b

// Pair

let pair a b = (a, b)
let swap (a, b) = (b, a)

let first f (a, b) = (f a, b)
let second f (a, b) = (a, f b)

// List

let cons x xs = x :: xs

let foldr f z xs = List.foldBack f xs z

let lookup x = List.tryFind (fst >> (=) x) >> Option.map snd

// Option

let option z f = function
  | None -> z
  | Some a -> f a

// Result

let explain e = option (Error e) Ok

let (>=>) f g = f >> Result.bind g
