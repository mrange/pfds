namespace pfds

module SkewBinaryRandomAccessList = 

    module TreeDetails =     
        type Tree<'T> = 
            | Leaf of 'T
            | Node of 'T * Tree<'T> * Tree<'T>

        let rec lookupTree (i : int) (s : int) (t : Tree<'T>) : 'T = 
            let d = s / 2
            match (i, s, t) with
            | (0, 1, Leaf v)                    -> v
            | (0, _, Node (v, _, _))            -> v
            | (_, _, Node (_, l, _)) when i < d -> lookupTree (i - 1) d l
            | (_, _, Node (_, _, r)) when i > d -> lookupTree (i - 1 - d) d r
            | _                                 -> failwithf "List out of bounds: %d" i
    
        let rec updateTree (i : int) (v : 'T) (s : int) (t : Tree<'T>) : Tree<'T> = 
            let d = s / 2
            match (i, s, t) with
            | (0, 1, Leaf _)                        -> Leaf v
            | (0, _, Node (_, l, r))                -> Node (v, l, r)
            | (_, _, Node (vv, l, rr)) when i < d   -> Node (vv, updateTree (i - 1) v d l, rr)
            | (_, _, Node (vv, ll, r)) when i > d   -> Node (vv, ll, updateTree (i - 1 - d) v d r)
            | _                                     -> failwithf "List out of bounds: %d" i

        let rec fillArrayFromTree (ra : ResizeArray<'T>) (t : Tree<'T>) : unit =
            match t with
            | Leaf v            -> ra.Add v
            | Node (v, l, r)    -> ra.Add v; fillArrayFromTree ra l; fillArrayFromTree ra r

    open TreeDetails

    type RAList<'T> = (int * TreeDetails.Tree<'T>) list

    module RAListDetails =
        let rec fillArrayFromRAList (ra : ResizeArray<'T>) (ral : RAList<'T>) : unit =
            match ral with
            | [] -> ()
            | (_, t)::ws -> TreeDetails.fillArrayFromTree ra t; fillArrayFromRAList ra ws

    open RAListDetails

    let empty : RAList<'T> = []

    let cons (v : 'T) (ral : RAList<'T>) : RAList<'T> = 
        match ral with
        | (i1, t1)::(i2, t2)::ws when i1 = i2    -> 
            (1 + i1 + i2, Node (v, t1, t2))::ws
        | _ -> (1, Leaf v)::ral

    let uncons (ral : RAList<'T>) : 'T*RAList<'T> = 
        match ral with
        | []                        -> failwith "uncons requires non-empty list"
        | (1, Leaf v)::ws           -> v,ws
        | (i, Node (v, t1, t2))::ws -> let d = i / 2 
                                       v,(d, t1)::(d, t2)::ws
        | _                         -> failwithf "RAList invariant broken: %A" ral

    let rec lookup (i : int) (ral : RAList<'T>) : 'T =
        match ral with
        | []                        -> failwithf "List out of bounds: %d" i
        | (i1, t1)::_ when i < i1   -> lookupTree i i1 t1
        | (i1, t1)::ws              -> lookup (i - i1) ws

    let rec update (i : int) (v : 'T) (ral : RAList<'T>) : RAList<'T> =
        match ral with
        | []                        -> failwithf "List out of bounds: %d" i
        | (i1, t1)::ws when i < i1  -> (i1, (updateTree i v i1 t1))::ws
        | (i1, t1)::ws              -> (i1, t1)::update (i - i1) v ws

    let toArray (ral : RAList<'T>) : 'T [] = 
        let ra = ResizeArray<'T> ()
        fillArrayFromRAList ra ral
        ra.ToArray ()

    let toList (ral : RAList<'T>) : List<'T> = 
        let ra = ResizeArray<'T> ()
        fillArrayFromRAList ra ral
        ra |> Seq.toList
   
   
