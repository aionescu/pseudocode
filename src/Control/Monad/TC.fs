module Control.Monad.TC

open Utils

// The "Type Checking" monad
// =~ ReaderT r (Either e) a
type TC<'e, 'r, 'a> = TC of ('r -> Result<'a, 'e>)

let runTC r (TC tc) = tc r

let pure' a = TC <| fun _ -> Ok a
let err e = TC <| fun _ -> Error e

let lift r = TC <| fun _ -> r

let ask = TC Ok
let asks f = TC (Ok << f)

let local f (TC tc) = TC (tc << f)

let mapErr f (TC tc) = TC (Result.mapError f << tc)

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

let when' b m = if b then m else pure' ()
let unless b m = if b then pure' () else m

let rec traverse f xs =
  match xs with
  | [] -> pure' []
  | x :: xs -> curry List.Cons <!> f x <*> traverse f xs

let rec traverse_ f xs =
  match xs with
  | [] -> pure' ()
  | x :: xs -> f x *> traverse_ f xs

let rec traversePair f (a, b) = pair a <!> f b

let rec traverseMap f xs =
  Map.toList xs
  |> traverse (traversePair f)
  <&> Map.ofList
