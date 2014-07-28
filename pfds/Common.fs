// ----------------------------------------------------------------------------------------------
// Copyright (c) Mårten Rånge.
// ----------------------------------------------------------------------------------------------
// This source code is subject to terms and conditions of the Microsoft Public License. A
// copy of the license can be found in the License.html file at the root of this distribution.
// If you cannot locate the  Microsoft Public License, please send an email to
// dlr@microsoft.com. By using this source code in any fashion, you are agreeing to be bound
//  by the terms of the Microsoft Public License.
// ----------------------------------------------------------------------------------------------
// You must not remove this notice, or any other, from this software.
// ----------------------------------------------------------------------------------------------

namespace pfds

open System.Threading

[<StructuralEquality>]
[<NoComparison>]
exception EmptyException

[<StructuralEquality>]
[<NoComparison>]
exception OutOfBoundsException      of int

[<StructuralEquality>]
[<NoComparison>]
exception InvariantBrokenException  of obj

/// Lazy in F# relies on Lazy<> initialized with LazyThreadSafetyMode.ExecutionAndPublication.
/// LazyThreadSafetyMode.ExecutionAndPublication implies long-running locks. This is the safe
/// option in the presence of side-effects. However, in a functional setting with no side-effects
/// it can be more efficient to allow multiple and unnecessary initalizations as this doesn't require
/// long-running locks
type SimplisticLazy<'T>() =

    [<VolatileField>]
    let mutable creator = Unchecked.defaultof<_>

    let mutable value   = Unchecked.defaultof<'T>

    new (creator : unit -> 'T) as x =
        SimplisticLazy<'T> ()
        then
            x.Creator <- creator

    new (v : 'T) as x =
        SimplisticLazy<'T> ()
        then
            x.Value <- v

    member x.Value
        with get () =
            let c = creator
            if not <| obj.ReferenceEquals (c, Unchecked.defaultof<_>) then
                value   <- c ()
                // This assumes intel CPU
                // Intel CPUs relies on MOV for non-atomic and atomic reads.
                // However there is a difference between non-atomic and atomic writes
                // By setting a memory barrier before we set the creator (guardian) we
                // make sure the writes of value bytes isn't reordered and should be
                // consistent when setting the creator (guardian) flag
                Thread.MemoryBarrier ()
                creator <- Unchecked.defaultof<_>

            value
        and private set v =
            value <- v

    member private x.Creator
        with get () = creator
        and  set  v = creator <- v

module SimplisticLazy =

    let inline create (creator : unit -> 'T) : SimplisticLazy<'T> = SimplisticLazy<'T>(creator)

    let inline createFromValue (v : 'T) : SimplisticLazy<'T> = SimplisticLazy<'T>(v)
