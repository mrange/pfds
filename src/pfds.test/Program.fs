open System
open System.Collections.Generic
open FsCheck
open pfds.ImplicitRecursiveSlowdown
open pfds.SkewBinary


#if DEBUG
let config = { Config.Quick with MaxTest = 10 }
#else
let config = { Config.Quick with MaxTest = 1000 }
#endif

module TestRAList =
  type RAListActions<'T> =
    | Lookup      of float
    | Push        of 'T
    | Pop

  let coerce v = 
    match v with
    | _ when Double.IsNaN v -> 0.
    | _ when Double.IsInfinity v -> 1.
    | _ -> v

  type Properties() =
    static member ``Compare RAList's behavior to oracle`` (actions : RAListActions<int> list) =
      let push (l : ResizeArray<'T>) v  = l.Add v
      let isEmpty (l : ResizeArray<'T>) = l.Count = 0
      let pop (l : ResizeArray<'T>)     = 
        let last  = l.Count - 1
        let v     = l.[last]
        l.RemoveAt last
        v
      let length (l : ResizeArray<'T>)  = l.Count
      let lookup (l : ResizeArray<'T>) i= l.[l.Count - 1 - i]

      let rec loop (oracle : ResizeArray<'T>) (ral : RAList.Type<'T>) actions =
        match actions with
        | [] -> true, ral
        | (Push v)::actions ->
          push oracle v
          loop oracle (RAList.cons v ral) actions
        | Pop::actions ->
          if oracle |> isEmpty |> not && ral |> RAList.isEmpty |> not then
            let e     = pop oracle
            let h, t  = RAList.headAndTail ral
            if e = h then loop oracle t actions
            else 
              false, ral
          else if isEmpty oracle && ral |> RAList.isEmpty then
            loop oracle ral actions
          else
            false, ral
        | (Lookup ir)::actions ->
          let l = length oracle
          if l = 0 then
            loop oracle ral actions
          elif l = RAList.length ral then
            let ir  = (ir |> coerce |> abs) % 1.0
            let i   = ir * float (l - 1) |> round |> int
            let e   = lookup oracle i
            let a   = RAList.lookup i ral 
            if e    = a then 
              loop oracle ral actions
            else 
              false, ral
          else
            false, ral
      
      let oracle    = ResizeArray<int> ()
      let r, ral    = loop oracle RAList.empty actions
      oracle.Reverse ()
      let expected  = oracle.ToArray ()
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
  type QueueActions<'T> =
    | Push of 'T
    | Pop

  type Properties() =
    static member ``Compare Queue's behavior to oracle`` (actions : QueueActions<int> list) =
      let push (q : Queue<'T>) v  = q.Enqueue v
      let isEmpty (q : Queue<'T>) = q.Count = 0
      let pop (q : Queue<'T>)     = q.Dequeue ()
      let rec loop (oracle : Queue<'T>) (q : Queue.Type<'T>) actions =
        match actions with
        | [] -> true, q
        | (Push v)::actions ->
          push oracle v
          loop oracle (Queue.snoc q v) actions
        | Pop::actions ->
          if oracle |> isEmpty |> not && q |> Queue.isEmpty |> not then
            let e     = pop oracle
            let h, t  = Queue.headAndTail q
            if e = h then loop oracle t actions
            else false, q
          else if isEmpty oracle && q |> Queue.isEmpty then
            loop oracle q actions
          else
            false, q

      let oracle    = Queue<int> ()
      let r, q      = loop oracle Queue.empty actions
      let expected  = oracle |> Seq.toArray
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
    TestQueue.test ()
    TestRAList.test ()
    0
  with
  | e -> 
    printfn "Exception: %s" e.Message
    999
