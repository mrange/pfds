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

open System.Diagnostics

module BootstrappedQueue =

    // TODO: Represent small queues as tuples

    type Queue<'T> = 
        | Empty
        | Short of int*'T*'T*'T*'T
        | Long  of 'T list*int*Queue<Lazy<'T list>>*'T list

    module Details =

        let isEmptyImpl (q : Queue<'T>) =
            match q with
            | Empty -> true
            | _     -> false

        let rec checkInnerInvariant<'T> (t : 'T list*int*Queue<Lazy<'T list>>*'T list) : Queue<'T> = 
            match t with
            | ([], _, Empty, _) -> Empty
            | ([], lenm, m, r)  ->
                let tot = lenm + r.Length
                if tot > 4 then
                    let h = (m |> headImpl).Value
                    let d = lenm - h.Length
                    let m = m |> tailImpl
                    Long (h, d, m, r)
                else
                    let mutable i  = 0
                    let mutable v0 = Unchecked.defaultof<'T>
                    let mutable v1 = Unchecked.defaultof<'T>
                    let mutable v2 = Unchecked.defaultof<'T>
                    let mutable v3 = Unchecked.defaultof<'T>

                    let mutable q = m
                    while not (isEmptyImpl q) do
                        let h = q |> headImpl
                        q <- q |> tailImpl 
                        let vs = h.Value
                        for v in vs do
                            match i with
                            | 0 -> v0 <- v
                            | 1 -> v1 <- v
                            | 2 -> v2 <- v
                            | 3 -> v3 <- v
                            | _ -> raise <| OutOfBoundsException i
                            i <- i + 1

                    for v in (r |> List.rev) do
                            match i with
                            | 0 -> v0 <- v
                            | 1 -> v1 <- v
                            | 2 -> v2 <- v
                            | 3 -> v3 <- v
                            | _ -> raise <| OutOfBoundsException i
                            i <- i + 1

                    Short (tot,v0,v1,v2,v3)
                    
            | _ -> Long t

        and checkInvariant<'T> (f : 'T list , lenm : int, m : Queue<Lazy<'T list>>, r : 'T list) : Queue<'T> = 
            // TODO: Remove unnecessary obj creation
            if r.Length <= f.Length + lenm then
                checkInnerInvariant (f, lenm, m, r)
            else
                let lenm    = (lenm + r.Length)
                let m       = snocImpl (lazy List.rev r, m)
                checkInnerInvariant (f, lenm, m, [])

        and snocImpl<'T> (v : 'T, q : Queue<'T>) : Queue<'T> = 
            match q with 
            | Empty                 -> Short (1, v, Unchecked.defaultof<_>, Unchecked.defaultof<_>, Unchecked.defaultof<_>)
            | Short (1,v0,_,_,_)    -> Short (2, v0, v, Unchecked.defaultof<_>, Unchecked.defaultof<_>)
            | Short (2,v0,v1,_,_)   -> Short (3, v0, v1, v, Unchecked.defaultof<_>)
            | Short (3,v0,v1,v2,_)  -> Short (4, v0, v1, v2, v)
            | Short (4,v0,v1,v2,v3) -> Long ([v0;v1;v2;v3;v], 0, Empty, [])
            | Long (f, lenm, m, r)  -> checkInvariant (f, lenm, m, v::r)

        and headImpl<'T> (q : Queue<'T>) : 'T = 
            match q with 
            | Empty                 -> raise EmptyException
            | Short (_,v,_,_,_)     -> v
            | Long (v::_, _, _, _)  -> v
            | _                     -> raise <| InvariantBrokenException q

        and tailImpl<'T> (q : Queue<'T>) : Queue<'T> = 
            match q with 
            | Empty                     -> raise EmptyException
            | Short (1,v0,_,_,_)        -> Empty
            | Short (2,_,v1,_,_)        -> Short (1, v1, Unchecked.defaultof<_>, Unchecked.defaultof<_>, Unchecked.defaultof<_>)
            | Short (3,_,v1,v2,_)       -> Short (2, v1, v2, Unchecked.defaultof<_>, Unchecked.defaultof<_>)
            | Short (4,_,v1,v2,v3)      -> Short (3, v1, v2, v3, Unchecked.defaultof<_>)
            | Long (_::vs, lenm, m, r)  -> checkInvariant (vs, lenm, m, r)
            | _                         -> raise <| InvariantBrokenException q

        let rec fillArrayFromQueue<'T> (push: 'T -> unit, q : Queue<'T>) : unit =
            match q with
            | Empty                     -> ()
            | Short (i, v0, v1, v2, v3) -> 
                if i > 0 then
                    push v0
                if i > 1 then
                    push v1
                if i > 2 then
                    push v2
                if i > 3 then
                    push v3
            | Long (f, lenm, m, r)      ->
                let p (lv : Lazy<'T list>) = 
                    let v = lv.Value
                    v |> List.iter push

                f |> List.iter push
                fillArrayFromQueue (p, m)
                r |> List.rev |> List.iter push
        

    open Details

    let empty = Empty

    let inline isEmpty q= isEmptyImpl q
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
   
   

