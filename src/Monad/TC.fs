module Monad.TC

open Utils

type TC<'e, 'r, 'a> = TC of ('r -> Result<'a, 'e>)

let runTC r (TC tc) = tc r

let pure' a = TC <| fun _ -> Ok a
let err e = TC <| fun _ -> Error e

let lift r = TC <| fun _ -> r

let ask = TC Ok
let asks f = TC (Ok << f)

let local f (TC tc) = TC (tc << f)

let (<!>) f (TC tc) = TC (Result.map f << tc)

let (<*>) (TC f) (TC a) = TC <| fun r ->
  match f r, a r with
  | Ok f, Ok a -> Ok (f a)
  | Error e, _ -> Error e
  | _, Error e -> Error e

let (=<<) f (TC tc) = TC <| fun r ->
  match tc r with
  | Ok a -> runTC r (f a)
  | Error e -> Error e

let (<&>) m f = f <!> m
let (&>) a v = const' v <!> a
let (<&) v a = a &> v

let ( *> ) a b = const' id <!> a <*> b
let ( <* ) a b = const' <!> a <*> b

let (>>=) a f = f =<< a
let join a = id =<< a

let (<|>) (TC a) (TC b) = TC <| fun r ->
  match a r with
  | Ok _ as a -> a
  | _ -> b r

let rec traverse f xs =
  match xs with
  | [] -> pure' []
  | x :: xs -> cons <!> f x <*> traverse f xs

let rec traverse_ f xs =
  match xs with
  | [] -> pure' ()
  | x :: xs -> f x *> traverse_ f xs

let sequence xs = traverse id xs
