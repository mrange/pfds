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

module BootstrappedQueue =

    // TODO: Uncurry helper functions

    type Queue<'T> = 
        | Empty
        | Queued of 'T list*int*Queue<Lazy<'T list>>*'T list

    module Details =
        let cutoff = 10

        let rec checkInnerInvariant<'T> (t : 'T list*int*Queue<Lazy<'T list>>*'T list) : Queue<'T> = 
            match t with
            | ([], _, Empty, _) -> Empty
            | ([], lenm, m, r)  ->
                let h = (m |> headImpl).Value
                let m = m |> tailImpl
                Queued (h, lenm - h.Length, m, r)
            | _ -> Queued t

        and checkInvariant<'T> (f : 'T list , lenm : int, m : Queue<Lazy<'T list>>, r : 'T list) : Queue<'T> = 
            if r.Length <= f.Length + lenm then
                checkInnerInvariant (f, lenm, m, r)
            else
                let lenm    = (lenm + r.Length)
                let m       = snocImpl m (lazy List.rev r)
                checkInnerInvariant (f, lenm, m, [])

        and snocImpl<'T> (q : Queue<'T>) (v : 'T) : Queue<'T> = 
            match q with 
            | Empty                     -> Queued ([v], 0, Empty, [])
            | Queued (f, lenm, m, r)    -> checkInvariant (f, lenm, m, v::r)

        and headImpl<'T> (q : Queue<'T>) : 'T = 
            match q with 
            | Empty                     -> raise EmptyException
            | Queued (v::_, _, _, _)    -> v
            | _                         -> raise <| InvariantBrokenException q

        and tailImpl<'T> (q : Queue<'T>) : Queue<'T> = 
            match q with 
            | Empty                         -> raise EmptyException
            | Queued (_::vs, lenm, m, r)    -> checkInvariant (vs, lenm, m, r)
            | _                             -> raise <| InvariantBrokenException q

    open Details

    let empty = Empty

    let inline snoc q v = snocImpl q v 
    let inline head q   = headImpl q
    let inline tail q   = tailImpl q
