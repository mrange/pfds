namespace pfds.test

module ReferenceImplementation = 
    
    type RAList<'T> = 'T list

    let empty : RAList<'T>  = []

    let cons (v : 'T) (ral : RAList<'T>) : RAList<'T> = v::ral
    let uncons (ral : RAList<'T>) : 'T*RAList<'T> = 
        match ral with
        | []    -> failwith "list must be non-empty"
        | x::xs -> x,xs

    let head (ral : RAList<'T>) = 
        let h,_ = uncons ral
        h

    let tail (ral : RAList<'T>) = 
        let _,t = uncons ral
        t
    
    let rec lookup (i : int) (ral : RAList<'T>) : 'T = 
        match i, ral with
        | _, []     -> failwith "List out of bound"
        | 0, x::_   -> x
        | i, _::xs  -> lookup (i - 1) xs

    let rec update (i : int) (v : 'T) (ral : RAList<'T>) : RAList<'T> = 
        match i, ral with
        | _, []     -> failwith "List out of bound"
        | 0, _::xs  -> v::xs
        | i, x::xs  -> x::update (i - 1) v xs

    let toList (ral : RAList<'T>) = ral

    let toArray (ral : RAList<'T>) = ral |> List.toArray
