module TestRAList =
  open System.Collections.Generic

  open pfds

  open FsCheck

  type Properties() =
    static member ``input |> fromSeq |> toSeq = input`` (input : int []) =
      let expected  = input
      let actual    = input |> RAList.fromSeq |> RAList.toSeq |> Seq.toArray
      expected = actual

  let test () =
    let config = { Config.Quick with MaxTest = 1000 }
    
    Check.All<Properties> config

module TestQueue =
  open System.Collections.Generic

  open pfds

  open FsCheck

  type QueueActions<'T> =
    | Push of 'T
    | Pop

  type Properties() =
    static member ``Compare queue's behavior to oracle`` (actions : QueueActions<int> list) =
      let rec loop (oracle : Queue<'T>) (q : Queue.Type<'T>) actions =
        match actions with
        | [] -> true, q
        | (Push v)::actions ->
          oracle.Enqueue v
          loop oracle (Queue.snoc q v) actions
        | Pop::actions ->
          if oracle.Count > 0 && q |> Queue.isEmpty |> not then
            let e     = oracle.Dequeue ()
            let h, t  = Queue.headAndTail q
            if e = h then loop oracle t actions
            else false, q
          else if oracle.Count = 0 && q |> Queue.isEmpty then
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
    let config = { Config.Quick with MaxTest = 1000 }
    
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
