module Utils.State

open Utils.Function

type State<'s, 'a> = 's -> 'a * 's

let runState s (f: State<'s, 'a>) = f s
let evalState s (f: State<'s, 'a>) = fst <| f s
let execState s (f: State<'s, 'a>) = snd <| f s

let get : State<'s, 's> = fun s -> s, s
let gets (f: 's -> 'a) : State<'s, 'a> = fun s -> f s, s
let put s : State<'s, unit> = fun _ -> (), s
let modify f : State<'s, unit> = fun s -> (), f s

let pure' a : State<'s, 'a> = fun s -> a, s

let (<!>) (f: 'a -> 'b) (m: State<'s, 'a>): State<'s, 'b> = m >> first f

let (<*>) (f: State<'s, 'a -> 'b>) (a: State<'s, 'a>): State<'s, 'b> = fun s ->
  let f, s = f s
  let a, s = a s
  f a, s

let (=<<) (f: 'a -> State<'s, 'b>) (m: State<'s, 'a>): State<'s, 'b> = m >> uncurry f

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
