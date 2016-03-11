module Common

open System
open System.Threading

// NoLockLazy depend on that the creator function can be called concurrently and multiple times
//  In that case NoLockLazy don't need to use locks to update the internal state
type NoLockLazy<'T>() =
  [<VolatileField>]
  let mutable valueOrCreate : obj = null

  new (creator : unit -> 'T) as x =
    NoLockLazy<'T> () then x.Creator <- creator

  member private x.Creator
    with  set (v : unit -> 'T) = valueOrCreate <- v
    
  member x.Value =
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

let inline DelayCreator (creator  : unit -> 'T) : NoLockLazy<'T> = NoLockLazy<'T> (creator)
let inline DelayValue   (value    : 'T        ) : NoLockLazy<'T> = DelayCreator (fun () -> value)

let (|Run|) (d : NoLockLazy<'T>) : 'T =
  Run d.Value



