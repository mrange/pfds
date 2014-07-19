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

namespace pfds.test

open System
open System.Diagnostics
open System.Linq

module Comparer = 

    [<StructuralEquality>]
    [<NoComparison>]
    exception UnsupportedOperation

    type CollectionAction = 
        | Cons
        | Snoc
        | Uncons
        | Head
        | Tail
        | Lookup
        | Update
        | ValidateContent

    type CollectionActionResult<'T,'C> = 
        | Value         of 'T*'C
        | Exception     of exn

    type CollectionAbstraction<'C> = 
        {
            Initial : seq<int>  -> 'C
            Cons    : int       -> 'C       -> 'C
            Snoc    : int       -> 'C       -> 'C
            Uncons  : 'C        -> int*'C
            Head    : 'C        -> int
            Tail    : 'C        -> 'C
            Lookup  : int       -> 'C       -> int
            Update  : int       -> int      -> 'C    -> 'C
            ToList  : 'C        -> int list
            ToArray : 'C        -> int []
        }
        static member NewRAList i c u l upd tl ta = 
            {
                Initial     = i
                Cons        = c
                Snoc        = fun _ c -> raise UnsupportedOperation
                Uncons      = u
                Head        = fun _ -> raise UnsupportedOperation
                Tail        = fun c -> raise UnsupportedOperation
                Lookup      = l
                Update      = upd
                ToList      = tl
                ToArray     = ta
            }

        static member NewQueue i s h t tl ta = 
            {
                Initial     = i
                Cons        = fun _ c -> raise UnsupportedOperation
                Snoc        = s
                Uncons      = fun c -> raise UnsupportedOperation
                Head        = h
                Tail        = t
                Lookup      = fun _ _  -> raise UnsupportedOperation
                Update      = fun _ _ c-> raise UnsupportedOperation
                ToList      = tl
                ToArray     = ta
            }
    let referenceRAList = 
        CollectionAbstraction<ReferenceImplementation.RAList.RAList<int>>.NewRAList 
            ReferenceImplementation.RAList.fromSeq
            ReferenceImplementation.RAList.cons 
            ReferenceImplementation.RAList.uncons
            ReferenceImplementation.RAList.lookup
            ReferenceImplementation.RAList.update
            ReferenceImplementation.RAList.toList
            ReferenceImplementation.RAList.toArray

    let referenceQueue = 
        CollectionAbstraction<ReferenceImplementation.Queue.Queue<int>>.NewQueue
            ReferenceImplementation.Queue.fromSeq
            ReferenceImplementation.Queue.snoc 
            ReferenceImplementation.Queue.head
            ReferenceImplementation.Queue.tail
            ReferenceImplementation.Queue.toList
            ReferenceImplementation.Queue.toArray

    let compare 
        (initialSize: int                       ) 
        (runs       : int                       ) 
        (actions    : (int*CollectionAction) [] )
        (name       : string                    )
        (ab1        : CollectionAbstraction<'C1>) 
        (ab2        : CollectionAbstraction<'C2>) =

        let random          = Random(19740531)

        let errors          = ref 0
        let last            = ref 0

        let length          = ref initialSize

        let cont            = ref true

        let rrun            = ref 0

        let distribution    = 
            [|
                for (f,a) in actions do
                    for i in 1..f do yield a
            |]

        let inc i           = 
            i := !i + 1
            !i

        let dec i           = 
            if !i = 0 then 0
            else
                i := !i - 1
                !i

        let getIdx l        =
            let i   = random.Next(0, l)
            // 1.05 causes some reads to go out of bounds, we want to test this too.
            1.05 * (float i) |> round |> int

        let getNext ()      = inc last

        let initial         = [| for i in 1..initialSize -> getNext () |] :> seq<int>
        let mc1             = ref (initial |> ab1.Initial)
        let mc2             = ref (initial |> ab2.Initial)

        let write c (prelude : string) (msg : string) = 
            let p = Console.ForegroundColor
            Console.ForegroundColor <- c
            try 
                Console.Write prelude
                Console.Write(String (' ', 10 - prelude.Length))
                Console.WriteLine(msg)
            finally
                Console.ForegroundColor <- p

        let info        (msg : string)  = write ConsoleColor.Gray "INFO" msg

        let highlight   (msg : string)  = write ConsoleColor.White "HIGHLIGHT" msg

        let success     (msg : string)  = write ConsoleColor.Green "SUCCESS" msg

        let error       (msg : string)  =
            ignore <| inc errors
            write ConsoleColor.Red "ERROR" msg

        let runAction (a : unit -> 'T*'C) : CollectionActionResult<'T,'C> = 
            try
                Value <| a ()
            with
                | exn -> Exception exn

        highlight <| sprintf "Starting test run: %s, running %d iterations" name runs

        while !cont && (!rrun < runs) do
            let run = !rrun
            ignore <| inc rrun

            // Used when debugging failures                        
            if run > Int32.MaxValue && Debugger.IsAttached then 
                let a1 = ab1.ToArray !mc1
                let a2 = ab2.ToArray !mc2
                info <| sprintf "a1 = %A" a1
                info <| sprintf "a2 = %A" a2
                Debugger.Break ()

            let pickAction  = random.Next (distribution.Length)
            let action      = distribution.[pickAction]

            let compareActions (a1 : unit -> 'T*'C1) (a2 : unit -> 'T*'C2) : bool = 
                let r1 = runAction a1
                let r2 = runAction a2
                match r1, r2 with
                | Exception e1  , Exception e2  when e1 = e2 -> 
                    true
                | Value (v1, c1), Value (v2,c2) when v1 = v2 -> 
                    mc1 := c1
                    mc2 := c2
                    true
                | Exception e1  , Exception e2  ->
                    error <| sprintf "%A(%i) operation inconsistency detected: R1=%A, R2=%A" action run e1 e2
                    true
                | Exception e1  , Value (v2, c2)->
                    error <| sprintf "%A(%i) operation inconsistency detected: R1=%A, R2=%A" action run e1 v2
                    mc2 := c2
                    false
                | Value (v1, c1), Exception e2  ->
                    error <| sprintf "%A(%i) operation inconsistency detected: R1=%A, R2=%A" action run v1 e2
                    mc1 := c1
                    false
                | Value (v1, c1), Value (v2, c2)-> 
                    error <| sprintf "%A(%i) operation inconsistency detected: R1=%A, R2=%A" action run v1 v2
                    mc1 := c1
                    mc2 := c2
                    false

            cont := 
                match action with
                | Cons ->
                    let i = getNext ()
                    let makeCons (ab : CollectionAbstraction<'C>) c = fun () -> (), ab.Cons i !c
                    if compareActions (makeCons ab1 mc1) (makeCons ab2 mc2) then
                        ignore <| inc length
                        true
                    else
                        false
                | Snoc ->
                    let i = getNext ()
                    let makeSnoc (ab : CollectionAbstraction<'C>) c = fun () -> (), ab.Snoc i !c
                    if compareActions (makeSnoc ab1 mc1) (makeSnoc ab2 mc2) then
                        ignore <| inc length
                        true
                    else
                        false
                | Uncons ->
                    let makeUncons (ab : CollectionAbstraction<'C>) c = fun () -> ab.Uncons !c
                    if compareActions (makeUncons ab1 mc1) (makeUncons ab2 mc2) then
                        ignore <| dec length
                        true
                    else
                        false

                | Head ->
                    let makeHead (ab : CollectionAbstraction<'C>) c = fun () -> ab.Head !c, !c
                    if compareActions (makeHead ab1 mc1) (makeHead ab2 mc2) then
                        ignore <| dec length
                        true
                    else
                        false
                        
                | Tail ->
                    let makeTail (ab : CollectionAbstraction<'C>) c = fun () -> (), ab.Tail !c
                    if compareActions (makeTail ab1 mc1) (makeTail ab2 mc2) then
                        ignore <| dec length
                        true
                    else
                        false
                        
                | Lookup ->
                    let idx = getIdx !length
                    let makeLookup (ab : CollectionAbstraction<'C>) c = fun () -> 
                        ab.Lookup idx !c, !c
                    compareActions (makeLookup ab1 mc1) (makeLookup ab2 mc2)
                | Update ->
                    let i   = getNext ()
                    let idx = getIdx !length
                    let makeUpdate (ab : CollectionAbstraction<'C>) c = fun () -> 
                        (), ab.Update idx i !c
                    compareActions (makeUpdate ab1 mc1) (makeUpdate ab2 mc2)
                | ValidateContent ->
                    let makeValidate (ab : CollectionAbstraction<'C>) c = fun () -> 
                        ab.ToArray !c, !c
                    compareActions (makeValidate ab1 mc1) (makeValidate ab2 mc2)

            if not !cont then 
                error <| "Detected non recoverable error, aborting test run"

        let a1              = ab1.ToArray !mc1
        let a2              = ab2.ToArray !mc2

        if a1 <> a2 then
            error <| "After test run the resulting collections are not equal"
        
        if !errors = 0 then
            success "Test run done, no errors detected"
        else
            error <| sprintf "Test run done, %d errors detected" !errors
            
        !errors = 0

    let compareToReferenceRAList 
        (initialSize: int                       ) 
        (runs       : int                       ) 
        (actions    : (int*CollectionAction) [] )
        (name       : string                    )
        (ab         : CollectionAbstraction<'C2>) =
        compare initialSize runs actions name referenceRAList ab

    let compareToReferenceQueue 
        (initialSize: int                       ) 
        (runs       : int                       ) 
        (actions    : (int*CollectionAction) [] )
        (name       : string                    )
        (ab         : CollectionAbstraction<'C2>) =
        compare initialSize runs actions name referenceQueue ab
