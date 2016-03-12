module pfds.Common

open System
open System.Threading

// Delayed depend on that the creator function can be called concurrently and multiple times
//  In that case Delayed don't need to use locks to update the internal state
type Delayed<'T>() =
  [<VolatileField>]
  let mutable valueOrCreate : obj = null

  new (creator : unit -> 'T) as x =
    Delayed<'T> () then x.Creator <- creator

  member private x.Creator
    with  set (v : unit -> 'T) = valueOrCreate <- v
    
  member x.Force =
    let vc = valueOrCreate
    match vc with
    | :? 'T                     as v  -> v
    | :? (unit -> 'T)           as cv ->
      let o   = cv () |> box
      let oo  = Interlocked.CompareExchange (&valueOrCreate, o, vc)
      if Object.ReferenceEquals (oo, vc) then
        // If we successfully replaced the creator
        o :?> 'T
      else  
        // If some other already replaced the creator, then use this value instead
        oo :?> 'T
    | _ ->
      failwith "Invalid case"

let inline DelayCreator (creator  : unit -> 'T) : Delayed<'T> = Delayed<'T> (creator)
let inline DelayValue   (value    : 'T        ) : Delayed<'T> = DelayCreator (fun () -> value)

let (|Force|) (d : Delayed<'T>) : 'T =
  Force d.Force

[<RequireQualifiedAccess>]
type StreamCell<'T> =
  | Empty
  | Cons  of 'T*Stream<'T>
and Stream<'T> = Delayed<StreamCell<'T>>

[<RequireQualifiedAccess>]
module Stream =
//  [<GeneralizableValue>]
  let empty () : Stream<'T> = DelayValue StreamCell.Empty

  module internal Details =
    let rec skipLoop (i : int) (s : Stream<'T>) () : StreamCell<'T> =
      match i, s with
      | 0, Force s                        -> s
      | _, Force StreamCell.Empty         -> StreamCell.Empty
      | n, Force (StreamCell.Cons (_,vs)) -> skipLoop (i - 1) vs ()

  open Details

  let isEmpty (s : Stream<'T>) : bool =
    match s with
    | Force StreamCell.Empty  -> true
    | _                       -> false

  let rec concat (l : Stream<'T>) (r : Stream<'T>) : Stream<'T> =
    match l with
    | Force (StreamCell.Cons (v, ls)) -> DelayCreator (fun () -> StreamCell.Cons (v, concat ls r))
    | Force StreamCell.Empty          -> r

  let rec take (i : int) (s : Stream<'T>) : Stream<'T> =
    match i, s with
    | 0, _                              -> empty ()
    | _, Force StreamCell.Empty         -> empty ()
    | n, Force (StreamCell.Cons (v,vs)) -> DelayCreator (fun () -> StreamCell.Cons (v, take (i - 1) vs))

  let skip (i : int) (s : Stream<'T>) : Stream<'T> =
    DelayCreator (skipLoop i s)

  let rec unfold (f : 'S -> ('T*'S) option) (s : 'S) : Stream<'T> =
    DelayCreator (fun () -> 
      match f s with
      | Some (vv, ss) -> StreamCell.Cons (vv, unfold f ss)
      | _ -> StreamCell.Empty)
    
