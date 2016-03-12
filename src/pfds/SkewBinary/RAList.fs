[<RequireQualifiedAccess>]
module pfds.SkewBinary.RAList

type Tree<'T> =
  | Leaf of 'T
  | Node of 'T*Tree<'T>*Tree<'T>

type Type<'T> = (int * Tree<'T>) list

module internal Details =
  let rec lengthLoop l s =
    match l with
    | []          -> s
    | (ts, _)::ls -> lengthLoop ls (s + ts)

  let rec treeLookup (hts : int) i (t : Tree<'T>) =
    match i, t with
    | 0, Leaf v1                            -> v1
    | 0, Node (v1, _, _)                    -> v1
    | _, Node (_ , lt, _) when i - 1 < hts  -> treeLookup (hts / 2) (i - 1) lt
    | _, Node (_ , _, rt)                   -> treeLookup (hts / 2) (i - 1 - hts) rt
    | _ -> failwith "lookup on invalid RAList"

open Details

[<GeneralizableValue>]
let empty : Type<'T> = []

let isEmpty (l : Type<'T>) : bool =
  match l with
  | []  -> true
  | _   -> false

let length (l : Type<'T>) : int =
  lengthLoop l 0

let headAndTail (l : Type<'T>) : 'T*Type<'T> =
  match l with
  | []                            -> failwith "headAndTail on empty RAList"
  | (1, Leaf v1)::rest            -> v1, rest
  | (ts, Node (v1, lt, rt))::rest -> v1, (ts / 2, lt)::(ts / 2, rt)::rest
  | _                             -> failwith "headAndTail on invalid RAList"

let cons (v : 'T) (l : Type<'T>) : Type<'T> =
  match l with
  | (ts1, t1)::(ts2, t2)::rest when ts1 = ts2 -> (1 + ts1 + ts2, Node (v, t1, t2))::rest
  | _                                         -> (1, Leaf v)::l

let rec lookup (i : int) (l : Type<'T>) : 'T =
  match l with
  | []                        -> failwith "lookup index out bounds"
  | (ts, t)::_    when i < ts -> treeLookup (ts / 2) i t
  | (ts, _)::rest             -> lookup (i - ts) rest

let toSeq (l : Type<'T>) : seq<'T> =
  seq {
    let mutable ml = l
    while not (isEmpty ml) do
      let hh, tt = headAndTail ml
      yield hh
      ml <- tt
  }

open System.Collections.Generic
open System.Linq

let fromSeq (s : seq<'T>) : Type<'T> =
  let rec loop (e : IEnumerator<'T>) l =
    if e.MoveNext () then
      loop e (cons e.Current l)
    else
      l
  let rs = s.Reverse ()
  use e = rs.GetEnumerator ()
  loop e empty
