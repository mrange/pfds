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
    
    type Queue<'T> = Stream.Stream<'T>*Stream.Stream<'T>

    [<GeneralizableValue>]
    let empty<'T> : Queue<'T> = Stream.empty<'T>, Stream.empty<'T>

    let isEmpty (q : Queue<'T>) : bool = 
        let f,_ = q
        Stream.isEmpty f

    module Details = 
        let check (q : Queue<'T>) : Queue<'T> = 
            let f,r = q
            let fl = Stream.length f
            let rl = Stream.length r
            if rl <= fl then q else (Stream.append f (Stream.reverse r),Stream.empty)

    let snoc v (q : Queue<'T>) : Queue<'T> = 
        let f,r = q
        let rr = Stream.cons v r
        Details.check (f,rr)

    let head (q : Queue<'T>) : 'T =
        let f,_ = q
        Stream.head f

    let tail (q : Queue<'T>) : Queue<'T> = 
        let f,r = q
        if Stream.isEmpty f then
            raise EmptyException
        else
            let ff = Stream.tail f
            Details.check (ff,r)

    let fromSeq (s : seq<'T>) : Queue<'T> =
        let f = Stream.fromSeq s
        f,Stream.empty

    let toSeq (q : Queue<'T>) : seq<'T> = 
        let f,r = q
        Seq.append (Stream.toSeq f) <| (Stream.toSeq r).Reverse ()

    let toList (q : Queue<'T>) : List<'T> = 
        q |> toSeq |> Seq.toList

    let toArray (q : Queue<'T>) : 'T [] = 
        q |> toSeq |> Seq.toArray
