// ----------------------------------------------------------------------------------------------
// Copyright 2015 Mårten Rånge
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
// ----------------------------------------------------------------------------------------------
open System
open FsCheck

module Test =
  open pfds.Common

  // Examples on different ways to implement a persistent queue in F#
  //  From Chris Okasaki's book "Purely Functional Data Structures"
  type TrivialQueue<'T>       = 'T list*'T list
  type PhysicistsQueue<'T>    = 'T list*int*Delayed<'T list>*int*'T list
  type BankersQueue<'T>       = int*Stream<'T>*int*Stream<'T>
  type RealTimeQueue<'T>      = Stream<'T>*'T list*Stream<'T>
  type BootstrappedQueue<'T>  =
    | Empty
    | Queue   of  int   * 'T list * BootstrappedQueue<Delayed<'T list>> * int * 'T list
  type Digit<'T> =
    | Zero
    | One   of 'T
    | Two   of 'T*'T
  type ImplicitRecursiveSlowdownQueue<'T> =
    | Shallow of Digit<'T>
    | Deep    of Digit<'T>*Delayed<ImplicitRecursiveSlowdownQueue<'T*'T>>*Digit<'T>

#if DEBUG
let config = { Config.Quick with MaxTest = 10 }
#else
let config = { Config.Quick with MaxTest = 1000 }
#endif

[<RequireQualifiedAccess>]
type QueueActions<'T> =
  | Push of 'T
  | Pop

[<RequireQualifiedAccess>]
type RAListActions<'T> =
  | Lookup      of float
  | Push        of 'T
  | Pop

module Oracles =
  open System.Collections.Generic
  open System.Linq

  let ralist<'T> () =
    let l           = ResizeArray<'T> ()
    let push v      = l.Add v
    let isEmpty ()  = l.Count = 0
    let pop ()      = 
      let last  = l.Count - 1
      let v     = l.[last]
      l.RemoveAt last
      v
    let length ()   = l.Count
    let lookup i    = l.[l.Count - 1 - i]
    let toArray ()  = (Enumerable.Reverse l).ToArray ()
    isEmpty, push, pop, length, lookup, toArray


  let queue<'T> ()  =
    let q           = Queue<'T> ()
    let push v      = q.Enqueue v
    let isEmpty ()  = q.Count = 0
    let pop ()      = q.Dequeue ()
    let toArray ()  = q.ToArray ()
    isEmpty, push, pop, toArray

module TestRAList =
  open pfds.SkewBinary

  let coerce v = 
    match v with
    | _ when Double.IsNaN v -> 0.
    | _ when Double.IsInfinity v -> 1.
    | _ -> v

  type Properties() =
    static member ``RAList's behavior is equivalent with to oracle's`` (actions : RAListActions<int> list) =
      let isEmpty, push, pop, length, lookup, toArray = Oracles.ralist ()

      let rec loop (ral : RAList.Type<int>) actions =
        match actions with
        | [] -> true, ral
        | (RAListActions.Push v)::actions ->
          push v
          loop (RAList.cons v ral) actions
        | RAListActions.Pop::actions ->
          if isEmpty () |> not && ral |> RAList.isEmpty |> not then
            let e     = pop ()
            let h, t  = RAList.headAndTail ral
            if e = h then loop t actions
            else 
              false, ral
          else if isEmpty () && ral |> RAList.isEmpty then
            loop ral actions
          else
            false, ral
        | (RAListActions.Lookup ir)::actions ->
          let l = length ()
          if l = 0 then
            loop ral actions
          elif l = RAList.length ral then
            let ir  = (ir |> coerce |> abs) % 1.0
            let i   = ir * float (l - 1) |> round |> int
            let e   = lookup i
            let a   = RAList.lookup i ral 
            if e = a then 
              loop ral actions
            else 
              false, ral
          else
            false, ral
      
      let r, ral    = loop RAList.empty actions
      let expected  = toArray ()
      let actual    = ral |> RAList.toSeq |> Seq.toArray
      if r && expected = actual then
        true
      else
        false

    static member ``input |> fromSeq |> toSeq = input`` (input : int []) =
      let expected  = input
      let actual    = input |> RAList.fromSeq |> RAList.toSeq |> Seq.toArray
      expected = actual

    static member ``RAList matches input length`` (input : int []) =
      let expected  = input.Length
      let actual    = input |> RAList.fromSeq |> RAList.length
      expected = actual


  let test () =
    Check.All<Properties> config

module TestQueue =
  open pfds.ImplicitRecursiveSlowdown

  type Properties() =
    static member ``Queue's behavior is equivalent with to oracle's`` (actions : QueueActions<int> list) =
      let isEmpty, push, pop, toArray = Oracles.queue<int> ()
      let rec loop (q : Queue.Type<int>) actions =
        match actions with
        | [] -> true, q
        | (QueueActions.Push v)::actions ->
          push v
          loop (Queue.snoc q v) actions
        | QueueActions.Pop::actions ->
          if isEmpty () |> not && q |> Queue.isEmpty |> not then
            let e     = pop ()
            let h, t  = Queue.headAndTail q
            if e = h then loop t actions
            else false, q
          else if isEmpty () && q |> Queue.isEmpty then
            loop q actions
          else
            false, q

      let r, q      = loop Queue.empty actions
      let expected  = toArray ()
      let actual    = q |> Queue.toSeq |> Seq.toArray
      r && expected = actual

    static member ``input |> fromSeq |> toSeq = input`` (input : int []) =
      let expected  = input
      let actual    = input |> Queue.fromSeq |> Queue.toSeq |> Seq.toArray
      expected = actual

  let test () =
    Check.All<Properties> config

module TestBootstappedQueue =
  open pfds.Bootstrapped

  type Properties() =
    static member ``Queue's behavior is equivalent with to oracle's`` (actions : QueueActions<int> list) =
      let isEmpty, push, pop, toArray = Oracles.queue<int> ()
      let rec loop (q : Queue.Type<int>) actions =
        match actions with
        | [] -> true, q
        | (QueueActions.Push v)::actions ->
          push v
          loop (Queue.snoc q v) actions
        | QueueActions.Pop::actions ->
          if isEmpty () |> not && q |> Queue.isEmpty |> not then
            let e     = pop ()
            let h, t  = Queue.headAndTail q
            if e = h then loop t actions
            else false, q
          else if isEmpty () && q |> Queue.isEmpty then
            loop q actions
          else
            false, q

      let r, q      = loop Queue.empty actions
      let expected  = toArray ()
      let actual    = q |> Queue.toSeq |> Seq.toArray
      r && expected = actual

    static member ``input |> fromSeq |> toSeq = input`` (input : int []) =
      let expected  = input
      let actual    = input |> Queue.fromSeq |> Queue.toSeq |> Seq.toArray
      expected = actual

  let test () =
    Check.All<Properties> config

[<EntryPoint>]
let main argv = 
  try
    TestBootstappedQueue.test ()
    TestQueue.test ()
    TestRAList.test ()
    0
  with
  | e -> 
    printfn "Exception: %s" e.Message
    999
