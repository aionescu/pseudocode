module Utils

let flip f a b = f b a
let const' a _ = a

let curry f a b = f (a, b)
let uncurry f (a, b) = f a b

let curry3 f a b c = f (a, b, c)
let curry4 f a b c d = f (a, b, c, d)
let curry5 f a b c d e = f (a, b, c, d, e)

let (>..) f g a b = g <| f a b
let (..<) f g = g >.. f

let cons x xs = x :: xs
let swap (a, b) = (b, a)

let first f (a, b) = (f a, b)
let second f (a, b) = (a, f b)
let pair a b = (a, b)

let rec foldr1 f = function
  | [] -> failwith "foldr1: Empty list"
  | [x] -> x
  | x :: xs -> f x (foldr1 f xs)

let explain e = function
  | None -> Error e
  | Some a -> Ok a
