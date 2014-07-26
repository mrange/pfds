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

module BatchedQueue =

    // Invariant: Non-empty queues front queue is non-empty
    type Queue<'T> = 'T list*'T list

    let empty : Queue<'T> = [], []

    let isEmpty (q : Queue<'T>) =
        match q with
        | [], []    -> true
        | _         -> false

    let snoc (v : 'T) (q : Queue<'T>) : Queue<'T> =
        match q with
        | [], []    -> [v], []
        | fs, rs    -> fs,v::rs

    let head (q : Queue<'T>) : 'T           =
        match q with
        | [], []    -> raise EmptyException
        | f::_,_    -> f
        | _         -> raise <| InvariantBrokenException q

    let tail (q : Queue<'T>) : Queue<'T>    =
        match q with
        | [], []    -> raise EmptyException
        | [f], rs   -> rs |> List.rev, []
        | _::fs, rs -> fs, rs
        | _         -> raise <| InvariantBrokenException q

    let fromSeq (s : seq<'T>) : Queue<'T>   = s |> Seq.toList,[]

    let toList ((fs, rs) : Queue<'T>)       = List.append fs (rs |> List.rev)

    let toArray (q : Queue<'T>)             = q |> toList |> List.toArray
