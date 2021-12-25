module Monad.State

open Utils

type State<'s, 'a> = State of ('s -> 'a * 's)

let runState s (State m) = m s
let evalState s (State m) = fst <| m s
let execState s (State m) = snd <| m s

let get () = State <| fun s -> s, s
let gets f = State <| fun s -> f s, s
let put s = State <| fun _ -> (), s
let modify f = State <| fun s -> (), f s

let pure' a = State <| fun s -> a, s

let (<!>) f (State m) = State (m >> first f)

let (<*>) (State f) (State a) = State <| fun s ->
  let f, s = f s
  let a, s = a s
  f a, s

let (=<<) f (State m) = State <| fun s ->
  let a, s = m s
  runState s (f a)

let (<&>) m f = f <!> m
let (>>=) m f = f =<< m

let (&>) a v = const' v <!> a
let (<&) v a = a &> v

let ( *> ) a b = const' id <!> a <*> b
let ( <* ) a b = const' <!> a <*> b

let rec traverse f xs =
  match xs with
  | [] -> pure' []
  | x :: xs -> cons <!> f x <*> traverse f xs

let rec traverse_ f xs =
  match xs with
  | [] -> pure' ()
  | x :: xs -> f x *> traverse_ f xs

let sequence xs = traverse id xs
