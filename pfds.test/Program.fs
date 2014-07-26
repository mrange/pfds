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
        CollectionAbstraction<BinaryRandomAccessList.RAList<int>>.NewRAList
            BinaryRandomAccessList.fromSeq
            BinaryRandomAccessList.isEmpty
            BinaryRandomAccessList.cons
            BinaryRandomAccessList.uncons
            BinaryRandomAccessList.lookup
            BinaryRandomAccessList.update
            BinaryRandomAccessList.toList
            BinaryRandomAccessList.toArray

    let sbral =
        CollectionAbstraction<SkewBinaryRandomAccessList.RAList<int>>.NewRAList
            SkewBinaryRandomAccessList.fromSeq
            SkewBinaryRandomAccessList.isEmpty
            SkewBinaryRandomAccessList.cons
            SkewBinaryRandomAccessList.uncons
            SkewBinaryRandomAccessList.lookup
            SkewBinaryRandomAccessList.update
            SkewBinaryRandomAccessList.toList
            SkewBinaryRandomAccessList.toArray

    let bq1 =
        CollectionAbstraction<BootstrappedQueue.Queue<int>>.NewQueue
            BootstrappedQueue.fromSeq
            BootstrappedQueue.isEmpty
            BootstrappedQueue.snoc
            BootstrappedQueue.head
            BootstrappedQueue.tail
            BootstrappedQueue.toList
            BootstrappedQueue.toArray

    let bq2 =
        CollectionAbstraction<BatchedQueue.Queue<int>>.NewQueue
            BatchedQueue.fromSeq
            BatchedQueue.isEmpty
            BatchedQueue.snoc
            BatchedQueue.head
            BatchedQueue.tail
            BatchedQueue.toList
            BatchedQueue.toArray

    let sq =
        CollectionAbstraction<StreamedQueue.Queue<int>>.NewQueue
            StreamedQueue.fromSeq
            StreamedQueue.isEmpty
            StreamedQueue.snoc
            StreamedQueue.head
            StreamedQueue.tail
            StreamedQueue.toList
            StreamedQueue.toArray

    let raactions =
        [|
            10  , IsEmpty
            10  , Cons
            10  , Uncons
            20  , Lookup
            10  , Update
            1   , ValidateContent
        |]

    let qactions =
        [|
            10  , IsEmpty
            10  , Snoc
            20  , Head
            10  , Tail
            1   , ValidateContent
        |]

    let iterations      = 10000

    let getName (n : string) (sz : int) = sprintf "%s, initial size: %d" n sz

    let ralist name initialSize ral = compareToReferenceRAList  initialSize iterations raactions (getName name initialSize) ral
    let queue  name initialSize q   = compareToReferenceQueue   initialSize iterations qactions  (getName name initialSize) q

    let initialSizes =
        [
            0
            10
            100
        ]

    for initialSize in initialSizes do
        ignore <| ralist    "SkewBinaryRandomAccessList"    initialSize sbral
        ignore <| ralist    "BinaryRandomAccessList"        initialSize bral
        ignore <| queue     "BootstrappedQueue"             initialSize bq1
        ignore <| queue     "BatchedQueue"                  initialSize bq2
        ignore <| queue     "StreamedQueue"                 initialSize sq

    0
