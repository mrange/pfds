[<RequireQualifiedAccess>]
module pfds.GG

type Tree<'T> =
  | Leaf of 'T
  | Node of 'T*Tree<'T>*Tree<'T>

type Type2<'T> = (int * Tree<'T>) list

[<GeneralizableValue>]
let empty : Type2<'T> = []

