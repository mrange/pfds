namespace pfds.test

open pfds

module ReferenceImplementation = 
    
    type RAList<'T> = 'T list

    module Details = 
        let rec lookupImpl (ri : int) (i : int) (ral : RAList<'T>) : 'T = 
            match i, ral with
            | _, []     -> raise <| OutOfBoundsException ri
            | 0, x::_   -> x
            | i, _::xs  -> lookupImpl ri (i - 1) xs

        let rec updateImpl (ri : int) (i : int) (v : 'T) (ral : RAList<'T>) : RAList<'T> = 
            match i, ral with
            | _, []     -> raise <| OutOfBoundsException ri
            | 0, _::xs  -> v::xs
            | i, x::xs  -> x::updateImpl ri (i - 1) v xs

    open Details

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

    let inline lookup i ral     = lookupImpl i i ral

    let inline update i v ral   = updateImpl i i v ral
    
    let toList (ral : RAList<'T>) = ral

    let toArray (ral : RAList<'T>) = ral |> List.toArray
