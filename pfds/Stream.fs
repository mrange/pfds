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

module Stream =

    type StreamCell<'T> =
        | StreamNil
        | StreamCons    of 'T*Stream<'T>
    and Stream<'T>      = SimplisticLazy<StreamCell<'T>>

    [<GeneralizableValue>]
    let inline empty<'T> : Stream<'T> = SimplisticLazy.create <| fun () -> StreamNil

    let inline (|Nil|Cons|) (s : Stream<'T>) =
        let sc = s.Value
        match sc with
        | StreamCons (vv,ss)    -> Cons (vv,ss)
        | StreamNil             -> Nil

    let isEmpty (s : Stream<'T>) : bool =
        match s.Value with
        | StreamNil -> true
        | _         -> false

    let cons v (s : Stream<'T>) : Stream<'T> =
        SimplisticLazy.create <| fun () -> StreamCons (v,s)

    let head (s : Stream<'T>) : 'T =
        match s.Value with
        | StreamNil         -> raise EmptyException
        | StreamCons (v,_)  -> v

    /// drop1 is almost like tail except it doesn't throw when "tailing" an empty Stream
    /// The reason for this is that it would require forcing the value too early
    /// When drop1 on empty Stream when forcing the value it will return an empty Stream
    let drop1 (s : Stream<'T>) : Stream<'T> =
        SimplisticLazy.create <| fun () ->
            match s.Value with
            | StreamNil         -> StreamNil
            | StreamCons (_,ss) -> ss.Value

    module Details =
        let rec takeImpl (n : int, s : Stream<'T>) : Stream<'T> =
            SimplisticLazy.create <| fun () ->
                match s.Value with
                | StreamNil             -> StreamNil
                | StreamCons (vv,ss)    -> StreamCons (vv, takeImpl (n - 1, ss))

        let rec dropImpl (n : int, s : Stream<'T>) : Stream<'T> =
            SimplisticLazy.create <| fun () ->
                if n = 0 then StreamNil
                else
                    match s.Value with
                    | StreamNil             -> StreamNil
                    | StreamCons (_,ss)     -> (dropImpl (n - 1, s)).Value

        let rec appendImpl (l : Stream<'T>, r : Stream<'T>) : Stream<'T> =
            SimplisticLazy.create <| fun () ->
                match l.Value with
                | StreamNil             -> r.Value
                | StreamCons (vv,ss)    -> StreamCons (vv, appendImpl (ss, r))

        let rec reverseImpl (r : Stream<'T>, s : Stream<'T>) : Stream<'T> =
            SimplisticLazy.create <| fun () ->
                match s.Value with
                | StreamNil             -> r.Value
                | StreamCons (vv, ss)   -> (reverseImpl (cons vv r, ss)).Value

    let inline append l r   = Details.appendImpl (l,r)
    let inline take n s     = Details.takeImpl (n,s)
    let inline drop n s     = Details.dropImpl (n,s)
    let inline reverse s    = Details.reverseImpl (empty, s)
    let inline ( ++ ) l r   = append l r

    let repeat (v : 'T) (n : int) : Stream<'T> =
        let i = ref 0

        let rec f () =
            if !i < n then
                i := !i + 1
                StreamCons (v, SimplisticLazy.create f)
            else StreamNil

        SimplisticLazy.create f

    let range (inclusiveFrom : int) (exclusiveTo : int) : Stream<int> =
        let i = ref inclusiveFrom

        let rec f () =
            if !i < exclusiveTo then
                i := !i + 1
                StreamCons (!i - 1, SimplisticLazy.create f)
            else StreamNil

        SimplisticLazy.create f
    // TOOD: Can this be rewritten as many appended streams where each substream is reversed
    //  In order to avoid a full reverse when forcing the first value
    let fromSeq (s : seq<'T>) : Stream<'T> =
        let mutable q = empty
        for v in s do
            q <- cons v q
        reverse q

(*
    A lazy implemention of fromSeq which unfortunately fails because
    it can't guarantee disposing the enumerator unless iterating to the end
    (Stream has no dipose concept)
    Implementation kept as a template for lazy streaming
    let fromSeq (s : seq<'T>) : Stream<'T> =
        let s = SimplisticLazy.create <| fun () ->
            let e = s.GetEnumerator ()
            let rec f () =
                if e.MoveNext () then
                    StreamCons (e.Current, SimplisticLazy.create f)
                else
                    e.Dispose ()
                    StreamNil
            f ()
        s
*)


    let toSeq (s : Stream<'T>) : seq<'T> =
        let s = ref s
        seq {
            while not <| isEmpty !s do
                let v = head !s
                yield v
                s := drop1 !s
        }

    let toList (s : Stream<'T>) : List<'T> =
        toSeq s |> Seq.toList

    let toArray (s : Stream<'T>) : 'T [] =
        toSeq s |> Seq.toArray