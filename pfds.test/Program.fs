
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
