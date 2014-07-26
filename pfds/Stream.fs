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
    and Stream<'T>      = Lazy<StreamCell<'T>>

    let empty<'T> = lazy StreamNil

    let cons v (s : Stream<'T>) : Stream<'T> = 
        lazy (StreamCons (v,s))

    module Details = 
        let rec takeImpl (n : int, s : Stream<'T>) : Stream<'T> = 
            lazy
                if n = 0 then StreamNil
                else
                    match s.Value with
                    | StreamNil             -> StreamNil
                    | StreamCons (vv,ss)    -> StreamCons (vv, takeImpl (n - 1, ss))

        let rec dropImpl (n : int, s : Stream<'T>) : Stream<'T> = 
            lazy
                if n = 0 then StreamNil
                else
                    match s.Value with
                    | StreamNil             -> StreamNil
                    | StreamCons (_,ss)     -> (dropImpl (n - 1, ss)).Value

        let rec appendImpl (l : Stream<'T>, r : Stream<'T>) : Stream<'T> = 
            lazy
                match l.Value with
                | StreamNil             -> r.Value
                | StreamCons (vv,ss)    -> 
                    StreamCons (vv, appendImpl (ss, r))

        let rec reverseImpl (r : Stream<'T>, s : Stream<'T>) : Stream<'T> = 
            lazy 
                match s.Value with
                | StreamNil             -> r.Value
                | StreamCons (vv, ss)   -> (reverseImpl (cons vv r, ss)).Value
    
    let inline append l r   = Details.appendImpl (l,r)
    let inline take n s     = Details.takeImpl (n,s)
    let inline drop n s     = Details.dropImpl (n,s)
    let inline reverse s    = Details.reverseImpl s
    let inline ( ++ ) l r   = append l r