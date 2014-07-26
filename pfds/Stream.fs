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
    and Stream<'T>      = int*Lazy<StreamCell<'T>>

    [<GeneralizableValue>]
    let inline empty<'T> : Stream<'T> = 0,lazy StreamNil

    let inline length (s : Stream<'T>) : int =
        let l,_ = s
        l

    let inline isEmpty (s : Stream<'T>) : bool = (length s) = 0

    let inline cons v (s : Stream<'T>) : Stream<'T> = 
        let l,_ = s
        l+1,lazy (StreamCons (v,s))

    let inline head (s : Stream<'T>) : 'T = 
        let l,lsc = s
        if l = 0 then raise EmptyException
        else
            match lsc.Value with
            | StreamCons (v,_)  -> v
            | _                 -> raise <| InvariantBrokenException s

    let inline tail (s : Stream<'T>) : Stream<'T> = 
        let l,lsc = s
        if l = 0 then raise EmptyException
        else
            l - 1, lazy
                match lsc.Value with
                | StreamCons (_,ss) -> 
                    let _,ilsc = ss
                    ilsc.Value
                | _                 -> raise <| InvariantBrokenException s

    module Details = 
        let rec takeImpl (n : int, s : Stream<'T>) : Stream<'T> = 
            let l,lsc = s
            (min l n), lazy
                if n = 0 then StreamNil
                else
                    match lsc.Value with
                    | StreamNil             -> StreamNil
                    | StreamCons (vv,ss)    -> StreamCons (vv, takeImpl (n - 1, ss))

        let rec dropImpl2 (n : int, lsc : Lazy<StreamCell<'T>>) : Lazy<StreamCell<'T>> = 
            lazy
                if n = 0 then StreamNil
                else
                    match lsc.Value with
                    | StreamNil             -> StreamNil
                    | StreamCons (_,ss)     -> 
                        let _,ilsc = ss
                        let dsc = dropImpl2 (n - 1, ilsc)
                        dsc.Value

        let dropImpl (n : int, s : Stream<'T>) : Stream<'T> = 
            let l,lsc = s
            (max (l - n) 0), dropImpl2 (n, lsc)

        let rec appendImpl (l : Stream<'T>, r : Stream<'T>) : Stream<'T> = 
            let ll,llsc = l
            let rl,rlsc = r
            ll + rl, lazy
                match llsc.Value with
                | StreamNil             -> rlsc.Value
                | StreamCons (vv,ss)    -> 
                    StreamCons (vv, appendImpl (ss, r))

        let rec reverseImpl2 (r : Stream<'T>, lsc : Lazy<StreamCell<'T>>) : Lazy<StreamCell<'T>> = 
            lazy 
                match lsc.Value with
                | StreamNil             -> 
                    let _,ilsc = r
                    ilsc.Value
                | StreamCons (vv, ss)   -> 
                    let _,ilsc = ss
                    (reverseImpl2 (cons vv r, ilsc)).Value

        let reverseImpl (s : Stream<'T>) : Stream<'T> = 
            let l,lsc = s
            l,reverseImpl2 (empty, lsc)


    let inline append l r   = Details.appendImpl (l,r)
    let inline take n s     = Details.takeImpl (n,s)
    let inline drop n s     = Details.dropImpl (n,s)
    let inline reverse s    = Details.reverseImpl s
    let inline ( ++ ) l r   = append l r

    // TODO: Implement as lazy?
    let fromSeq (s : seq<'T>) : Stream<'T> = 
        let mutable q = empty
        for v in s do
            q <- cons v q
        q
    
    let toSeq (s : Stream<'T>) : seq<'T> = 
        let s = ref s
        seq {
            while not <| isEmpty !s do
                let v = head !s
                yield v
                s := tail !s
        }