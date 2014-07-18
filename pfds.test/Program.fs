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

open System

open pfds
open pfds.test

open Comparer

[<EntryPoint>]
let main argv = 
    
    let bral = 
        CollectionAbstraction<BinaryRandomAccessList.RAList<int>>.New 
            BinaryRandomAccessList.empty
            BinaryRandomAccessList.cons 
            BinaryRandomAccessList.uncons
            BinaryRandomAccessList.lookup
            BinaryRandomAccessList.update
            BinaryRandomAccessList.toList
            BinaryRandomAccessList.toArray

    let sbral = 
        CollectionAbstraction<SkewBinaryRandomAccessList.RAList<int>>.New 
            SkewBinaryRandomAccessList.empty
            SkewBinaryRandomAccessList.cons 
            SkewBinaryRandomAccessList.uncons
            SkewBinaryRandomAccessList.lookup
            SkewBinaryRandomAccessList.update
            SkewBinaryRandomAccessList.toList
            SkewBinaryRandomAccessList.toArray

    let actions = 
        [|
            10  , Cons
            10  , Uncons
            20  , Lookup
            10  , Update
            1   , ValidateContent
        |]

    let test name ral = compareToReference 10 10000 actions name ral

    ignore <| test "SkewBinaryRandomAccessList" sbral
    ignore <| test "BinaryRandomAccessList"     bral

    0
