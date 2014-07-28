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

[<AutoOpen>]
module Common =

    let totalErrors = ref 0

    let inc i           =
        i := !i + 1
        !i

    let dec i           =
        if !i = 0 then 0
        else
            i := !i - 1
            !i

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
        ignore <| inc totalErrors
        write ConsoleColor.Red "ERROR" msg

    let testEqual (testScope : string) (testCase : string) (expected : 'T) (actual : 'T) : bool =
        if expected <> actual then
            error <| sprintf "%s.%s - %A (expected) <> %A (actual)" testScope testCase expected actual
            false
        else
            true


