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

open System.Linq

module BinaryRandomAccessList =

    type RAList<'T> =
        | Nil
        | Zero  of RAList<'T*'T>
        | One   of 'T*RAList<'T*'T>

    module Details =
        let rec consImpl<'T> (v : 'T, ral : RAList<'T>) : RAList<'T> =
            match ral with
            | Nil                   -> One (v, Nil)
            | Zero tt               -> One (v, tt)
            | One (vv, tt)          -> Zero <| consImpl ((v, vv), tt)

        let rec unconsImpl<'T> (ral : RAList<'T>) : 'T*RAList<'T> =
            match ral with
            | Nil                   -> raise EmptyException
            | One (vv, Nil)         -> vv, Nil
            | One (vv, tt)          -> vv, Zero tt
            | Zero tt               ->
                let (l,r),t = unconsImpl tt
                l, One (r, t)

        let rec lookupImpl<'T> (ri : int, i : int, ral : RAList<'T>) : 'T =
            match i,ral with
            | _, Nil                -> raise <| OutOfBoundsException ri
            | 0, One (vv, _)        -> vv
            | i, One (_, tt)        ->
                let i = i - 1
                let l,r = lookupImpl (ri, (i / 2), tt)
                if i % 2 = 0 then l else r
            | i, Zero tt            ->
                let l,r = lookupImpl (ri, (i / 2), tt)
                if i % 2 = 0 then l else r

        // TODO: Remove unnecessary obj creation
        let rec updateImpl<'T> (ri : int, i : int, u : 'T->'T, ral : RAList<'T>) : RAList<'T> =
            match i,ral with
            | _, Nil                -> raise <| OutOfBoundsException ri
            | 0, One (vv, tt)       -> One (u vv, tt)
            | i, One (vv, tt)       -> consImpl (vv, updateImpl (ri, (i - 1), u, Zero tt))
            | i, Zero tt            ->
                let ff (l,r) = if i % 2 = 0 then u l, r else l, u r
                Zero (updateImpl (ri, (i / 2), ff, tt))

        let rec fillArrayFromRAList<'T> (push : 'T -> unit, ral : RAList<'T>) : unit =
            match ral with
            | Nil           -> ()
            | Zero tt       ->
                fillArrayFromRAList ((fun (l,r) -> push l; push r), tt)
            | One (v, tt)   ->
                push v
                fillArrayFromRAList ((fun (l,r) -> push l; push r), tt)

    open Details

    let empty                       = Nil
    let isEmpty (ral : RAList<'T>)  =
        match ral with
        | Nil   -> true
        | _     -> false
    let inline cons v ral           = consImpl (v, ral)
    let inline uncons ral           = unconsImpl ral
    let inline lookup i ral         = lookupImpl (i, i, ral)
    let inline update i v ral       = updateImpl (i, i, (fun _ -> v), ral)

    let fromSeq (s : seq<'T>) : RAList<'T>   =
        let mutable ral = empty
        for v in s.Reverse () do
            ral <- ral |> cons v
        ral

    let toArray (ral : RAList<'T>) : 'T [] =
        let ra = ResizeArray<'T> ()
        fillArrayFromRAList (ra.Add, ral)
        ra.ToArray ()

    let toList (ral : RAList<'T>) : List<'T> =
        let ra = ResizeArray<'T> ()
        fillArrayFromRAList (ra.Add, ral)
        ra |> Seq.toList


