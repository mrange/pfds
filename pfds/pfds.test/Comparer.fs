namespace pfds.test

open System
open System.Diagnostics
open System.Linq

module Comparer = 

    type CollectionAction = 
        | Cons
        | Uncons
        | Lookup
        | Update
        | ValidateContent

    type CollectionActionResult<'T,'C> = 
        | Value         of 'T*'C
        | Exception     of exn

    type CollectionAbstraction<'C> = 
        {
            Empty   : 'C
            Cons    : int   -> 'C       -> 'C
            Uncons  : 'C    -> int*'C
            Lookup  : int   -> 'C       -> int
            Update  : int   -> int      -> 'C    -> 'C
            ToList  : 'C    -> int list
            ToArray : 'C    -> int []
        }
        static member New e c u l upd tl ta = 
            {
                Empty       = e
                Cons        = c
                Uncons      = u
                Lookup      = l
                Update      = upd
                ToList      = tl
                ToArray     = ta
            }

    let reference = 
        CollectionAbstraction<ReferenceImplementation.RAList<int>>.New 
            ReferenceImplementation.empty
            ReferenceImplementation.cons 
            ReferenceImplementation.uncons
            ReferenceImplementation.lookup
            ReferenceImplementation.update
            ReferenceImplementation.toList
            ReferenceImplementation.toArray

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

        let mc1             = ref ab1.Empty
        let mc2             = ref ab2.Empty

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

        for i in 1..initialSize do
            let i = getNext ()
            mc1 := ab1.Cons i !mc1
            mc2 := ab2.Cons i !mc2
        
        while !cont && (!rrun < runs) do
            let run = !rrun
            ignore <| inc rrun

            if run = -1 && Debugger.IsAttached then 
                Debugger.Break ()
                let a1 = ab1.ToArray !mc1
                let a2 = ab2.ToArray !mc2
                info <| sprintf "a1 = %A" a1
                info <| sprintf "a2 = %A" a2

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
                | Uncons ->
                    let makeUncons (ab : CollectionAbstraction<'C>) c = fun () -> ab.Uncons !c
                    if compareActions (makeUncons ab1 mc1) (makeUncons ab2 mc2) then
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

    let compareToReference 
        (initialSize: int                       ) 
        (runs       : int                       ) 
        (actions    : (int*CollectionAction) [] )
        (name       : string                    )
        (ab         : CollectionAbstraction<'C2>) =
        compare initialSize runs actions name reference ab
