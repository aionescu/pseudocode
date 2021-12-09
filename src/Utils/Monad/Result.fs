module Utils.Monad.Result

open Utils.Function

let (<!>) = Result.map
let (<&>) a f = f <!> a

let (&>) a v = const' v <!> a
let (<&) v a = a &> v

let (<*>) f a =
  match f, a with
  | Ok f, Ok a -> Ok <| f a
  | Error e, _ -> Error e
  | _, Error e -> Error e

let ( *> ) a b = const' id <!> a <*> b
let ( <* ) a b = const' <!> a <*> b

let (=<<) = Result.bind
let (>>=) a f = f =<< a

let join a = a >>= id

let rec traverse f xs =
  match xs with
  | [] -> Ok []
  | x :: xs -> cons <!> f x <*> traverse f xs

let rec traverse_ f xs =
  match xs with
  | [] -> Ok ()
  | x :: xs -> f x *> traverse_ f xs

let sequence xs = traverse id xs

let explain e = function
  | None -> Error e
  | Some a -> Ok a

let hush = function
  | Error _ -> None
  | Ok a -> Some a
