[<RequireQualifiedAccess>]
module pfds.Bootstrapped.Queue

open pfds.Common

type Type<'T> =
  | Empty   //  lenfm   f         m                        lenr  r
  | Queue   of  int   * 'T list * Type<Delayed<'T list>> * int * 'T list

[<GeneralizableValue>]
let empty = Empty

let isEmpty (q : Type<'T>) : bool =
  match q with
  | Empty -> true
  | _     -> false

let rec internal checkQ<'T> (lenfm : int) (f : 'T list) (m : Type<Delayed<'T list>>) (lenr : int) (r : 'T list) : Type<'T> =
  if lenr < lenfm then checkF lenfm f m lenr r
  elif lenr = 0 then checkF (lenfm + lenr) f Empty 0 []
  else checkF (lenfm + lenr) f (snoc m (DelayCreator (fun () -> List.rev r))) 0 []
and internal checkF<'T> (lenfm : int) (f : 'T list) (m : Type<Delayed<'T list>>) (lenr : int) (r : 'T list) : Type<'T> =
  match f, m with
  | [], Empty -> empty
  | [], _ -> 
    let Force hh, tt = headAndTail m
    Queue (lenfm, hh, tt, lenr, r)
  | _ -> Queue (lenfm, f, m, lenr, r)
and snoc<'T> (q : Type<'T>) (v : 'T) : Type<'T> =
  match q with
  | Empty -> Queue (1, [v], Empty, 0, [])
  | Queue (lenfm, f, m, lenr, r) ->
    checkQ lenfm f m (lenr + 1) (v::r)
and headAndTail<'T> (q : Type<'T>) : 'T*Type<'T> =
  match q with
  | Empty -> failwith "headAndTail on empty Queue"
  | Queue (lenfm, f::fs, m, lenr, r) ->
    f, checkQ (lenfm - 1) fs m lenr r
  | _ -> failwith "headAndTail on invalid Queue"

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
