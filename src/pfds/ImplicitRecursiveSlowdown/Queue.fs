[<RequireQualifiedAccess>]
module pfds.ImplicitRecursiveSlowdown.Queue

open pfds.Common

type Digit<'T> =
  | Zero
  | One   of 'T
  | Two   of 'T*'T

type Type<'T> =
  | Shallow of Digit<'T>
  | Deep    of Digit<'T>*NoLockLazy<Type<'T*'T>>*Digit<'T>

[<GeneralizableValue>]
let empty : Type<'T> = Shallow Zero

let inline isEmpty (q : Type<'T>) : bool =
  match q with
  | Shallow Zero  -> true
  | _             -> false

let rec snoc<'T> (q : Type<'T>) (v : 'T) : Type<'T> =
  match q with
  | Shallow Zero                    -> Shallow (One v)
  | Shallow (One v1)                -> Shallow (Two (v1,v))
  | Shallow d                       -> Deep (d, DelayValue empty, One v)
  | Deep (ld, drq, Zero)            -> Deep (ld, drq, One v)
  | Deep (ld, drq, One v1)          -> Deep (ld, drq, Two (v1,v))
  | Deep (ld, Run rq, Two (v1, v2)) -> Deep (ld, DelayCreator (fun () -> snoc rq (v1, v2)), One v)

let rec headAndTail<'T> (q : Type<'T>) : 'T*Type<'T> =
  match q with
  | Shallow Zero                    -> failwith "headAndTail on empty Queue<'T>"
  | Shallow (One v1)                -> v1, empty
  | Shallow (Two (v1, v2))          -> v1, Shallow (One v2)
  | Deep (Zero, Run rq, td)         ->
    match rq, td with
    | Shallow Zero, Zero            -> failwith "headAndTail called on an invalid Queue<'T>"
    | Shallow Zero, One v1          -> v1, empty
    | Shallow Zero, Two (v1, v2)    -> v1, Shallow (One v2)
    | _ -> 
      let (v1, v2), tt = headAndTail rq
      v1, Deep (One v2, DelayValue tt, td)
  | Deep (One v1, drq, td)          -> v1, Deep (Zero, drq, td)
  | Deep (Two (v1, v2), drq, td)    -> v1, Deep (One v2, drq, td)

let rec isValid<'T> (q : Type<'T>) : bool =
  match q with
  | Shallow _               -> true
  | Deep (_, _, Zero)       -> false
  | Deep (_, Run rq, _)     -> isValid rq

let toSeq (q : Type<'T>) : seq<'T> =
  seq {
    let mutable mq = q
    while not (isEmpty mq) do
      let hh, tt = headAndTail mq
      yield hh
      mq <- tt
  }

open System.Collections.Generic

let fromSeq (s : seq<'T>) : Type<'T> =
  let rec loop (e : IEnumerator<'T>) q =
    if e.MoveNext () then
      loop e (snoc q e.Current)
    else
      q
  use e = s.GetEnumerator ()
  loop e empty
