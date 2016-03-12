// ----------------------------------------------------------------------------------------------
// Copyright 2015 Mårten Rånge
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
// ----------------------------------------------------------------------------------------------
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

let inline (|Force|) (d : Delayed<'T>) : 'T =
  Force d.Force

[<RequireQualifiedAccess>]
type StreamCell<'T> =
  | Empty
  | Cons  of 'T*Stream<'T>
and [<RequireQualifiedAccess>] Stream<'T> = Delayed<StreamCell<'T>>

[<RequireQualifiedAccess>]
module Stream =
//  [<GeneralizableValue>]
  let inline empty () : Stream<'T> = DelayValue StreamCell.Empty

  let inline cons (v : 'T) (s : Stream<'T>) : Stream<'T> =
    DelayCreator (fun () -> StreamCell.Cons (v, s))

  let inline delay (fs : unit -> Stream<'T>) : Stream<'T> =
    DelayCreator (fun () -> let s = fs () in s.Force)

  module internal Details =
    let rec skipLoop (i : int) (s : Stream<'T>) () : Stream<'T> =
      match i, s with
      | 0, _                              -> s
      | _, Force StreamCell.Empty         -> empty ()
      | n, Force (StreamCell.Cons (_,vs)) -> skipLoop (i - 1) vs ()

    let rec reverseLoop (f : Stream<'T>) (t : Stream<'T>) () : Stream<'T> =
      match f with
      | Force StreamCell.Empty            -> t
      | Force (StreamCell.Cons (v,vs))    -> reverseLoop vs (cons v t) ()

  open Details

  let inline isEmpty (s : Stream<'T>) : bool =
    match s with
    | Force StreamCell.Empty  -> true
    | _                       -> false

  let inline (|Empty|Cons|) (s : Stream<'T>) =
    match s with
    | Force StreamCell.Empty          -> Empty
    | Force (StreamCell.Cons (v,vs))  -> Cons (v, vs)

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
    skipLoop i s |> delay

  let reverse (s : Stream<'T>) : Stream<'T> =
    reverseLoop s (empty ()) |> delay

  let rec unfold (f : 'S -> ('T*'S) option) (s : 'S) : Stream<'T> =
    DelayCreator (fun () -> 
      match f s with
      | Some (vv, ss) -> StreamCell.Cons (vv, unfold f ss)
      | _ -> StreamCell.Empty)
