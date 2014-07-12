namespace pfds.test

open pfds

module ReferenceImplementation = 
    
    type RAList<'T> = 'T list

    let empty : RAList<'T>  = []

    let cons (v : 'T) (ral : RAList<'T>) : RAList<'T> = v::ral
    let uncons (ral : RAList<'T>) : 'T*RAList<'T> = 
        match ral with
        | []    -> raise EmptyException
        | x::xs -> x,xs

    let head (ral : RAList<'T>) = 
        let h,_ = uncons ral
        h

    let tail (ral : RAList<'T>) = 
        let _,t = uncons ral
        t
    
    let rec lookup (i : int) (ral : RAList<'T>) : 'T = 
        match i, ral with
        | _, []     -> raise <| OutOfBoundsException i
        | 0, x::_   -> x
        | i, _::xs  -> lookup (i - 1) xs

    let rec update (i : int) (v : 'T) (ral : RAList<'T>) : RAList<'T> = 
        match i, ral with
        | _, []     -> raise <| OutOfBoundsException i
        | 0, _::xs  -> v::xs
        | i, x::xs  -> x::update (i - 1) v xs

    let toList (ral : RAList<'T>) = ral

    let toArray (ral : RAList<'T>) = ral |> List.toArray
