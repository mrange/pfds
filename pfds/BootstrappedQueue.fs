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

    // TODO: Represent small queues as tuples

    type Queue<'T> = 
        | Empty
        | Queued of 'T list*int*Queue<Lazy<'T list>>*'T list

    module Details =
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
                let m       = snocImpl (lazy List.rev r, m)
                checkInnerInvariant (f, lenm, m, [])

        and snocImpl<'T> (v : 'T, q : Queue<'T>) : Queue<'T> = 
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

        let rec fillArrayFromQueue<'T> (push: 'T -> unit, q : Queue<'T>) : unit =
            match q with
            | Empty                     -> ()
            | Queued (f, lenm, m, r)    ->
                let p (lv : Lazy<'T list>) = 
                    let v = lv.Value
                    v |> List.iter push

                f |> List.iter push
                fillArrayFromQueue (p, m)
                r |> List.rev |> List.iter push
        

    open Details

    let empty = Empty

    let inline snoc v q = snocImpl (v, q)
    let inline head q   = headImpl q
    let inline tail q   = tailImpl q

    let fromSeq (s : seq<'T>) : Queue<'T>   =
        let mutable q = empty
        for v in s do
            q <- q |> snoc v
        q

    let toArray (q : Queue<'T>) : 'T []     = 
        let ra = ResizeArray<'T> ()
        fillArrayFromQueue (ra.Add, q)
        ra.ToArray ()

    let toList (q : Queue<'T>) : List<'T>   = 
        let ra = ResizeArray<'T> ()
        fillArrayFromQueue (ra.Add, q)
        ra |> Seq.toList
   
   

