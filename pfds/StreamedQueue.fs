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

module StreamedQueue =

    type Queue<'T> = int*Stream.Stream<'T>*int*Stream.Stream<'T>

    [<GeneralizableValue>]
    let empty<'T> : Queue<'T> = 0,Stream.empty<'T>,0,Stream.empty<'T>

    let isEmpty (q : Queue<'T>) : bool =
        let fl,_,_,_ = q
        fl = 0

    module Details =
        let check (q : Queue<'T>) : Queue<'T> =
            let fl,f,rl,r = q
            if rl <= fl then q else rl + fl,Stream.append f (Stream.reverse r),0,Stream.empty

    let snoc v (q : Queue<'T>) : Queue<'T> =
        let fl,f,rl,r = q
        let rr = Stream.cons v r
        Details.check (fl,f,rl + 1,rr)

    let head (q : Queue<'T>) : 'T =
        let _,f,_,_ = q
        match f with
        | Stream.Nil            -> raise EmptyException
        | Stream.Cons (vv,_)    -> vv

    let tail (q : Queue<'T>) : Queue<'T> =
        let fl,f,rl,r = q
        match f with
        | Stream.Nil            -> raise EmptyException
        | Stream.Cons (_,ss)    ->
            Details.check (fl - 1,ss,rl,r)

    let fromSeq (s : seq<'T>) : Queue<'T> =
        let a = s |> Seq.toArray
        let f = a |> Stream.fromSeq
        a.Length,f,0,Stream.empty

    let toSeq (q : Queue<'T>) : seq<'T> =
        let _,f,_,r = q
        Seq.append (f |> Stream.toSeq) <| (r |> Stream.toSeq).Reverse ()

    let toList (q : Queue<'T>) : List<'T> =
        q |> toSeq |> Seq.toList

    let toArray (q : Queue<'T>) : 'T [] =
        q |> toSeq |> Seq.toArray
