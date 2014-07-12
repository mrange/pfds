namespace pfds

module BinaryRandomAccessList =

    type RAList<'T> = 
        | Nil
        | Zero  of RAList<'T*'T>
        | One   of 'T*RAList<'T*'T>

    module Details = 
        let rec consImpl<'T> (v : 'T) (ral : RAList<'T>) : RAList<'T> =
            match ral with
            | Nil                   -> One (v, Nil)
            | Zero tt               -> One (v, tt) 
            | One (vv, tt)          -> Zero <| consImpl (v,vv) tt

        let rec unconsImpl<'T> (ral : RAList<'T>) : 'T*RAList<'T> =
            match ral with
            | Nil                   -> raise EmptyException
            | One (vv, tt)          -> vv, Zero tt
            | Zero tt               -> 
                let (l,r),t = unconsImpl tt
                l, One (r, t)

        // TODO: Remove unnecessary obj creation
        let rec lookupImpl<'T> (ri : int) (i : int) (ral : RAList<'T>) : 'T =
            match i,ral with
            | _, Nil                -> raise <| OutOfBoundsException ri
            | 0, One (vv, _)        -> vv
            | i, One (_, tt)        -> lookupImpl ri (i - 1) (Zero tt)
            | i, Zero tt            -> 
                let l,r = lookupImpl ri (i / 2) tt
                if i % 2 = 0 then l else r

        // TODO: Restore O (log n)
        // TODO: Remove unnecessary obj creation
        let rec updateImpl<'T> (ri : int) (i : int) (v : 'T) (ral : RAList<'T>) : RAList<'T> =
            match i,ral with
            | _, Nil                -> raise <| OutOfBoundsException ri
            | 0, One (_, tt)        -> One (v, tt)
            | i, One (vv, tt)       -> consImpl vv <| updateImpl ri (i - 1) v (Zero tt)
            | i, Zero tt            -> 
                let l,r = lookupImpl ri (i / 2) tt
                let p = if i % 2 = 0 then v, r else l, v
                Zero <| updateImpl ri (i / 2) p tt

        let rec fillArrayFromRAList<'T> (push : 'T -> unit) (ral : RAList<'T>) : unit =
            match ral with
            | Nil           -> ()
            | Zero tt       -> 
                fillArrayFromRAList (fun (l,r) -> push l; push r) tt
            | One (v, tt)   -> 
                push v
                fillArrayFromRAList (fun (l,r) -> push l; push r) tt
                   
    open Details

    let empty                   = Nil
    let inline cons v ral       = consImpl v ral
    let inline uncons ral       = unconsImpl ral
    let inline lookup i ral     = lookupImpl i i ral
    let inline update i v ral   = updateImpl i i v ral

    let toArray (ral : RAList<'T>) : 'T [] = 
        let ra = ResizeArray<'T> ()
        fillArrayFromRAList ra.Add ral
        ra.ToArray ()

    let toList (ral : RAList<'T>) : List<'T> = 
        let ra = ResizeArray<'T> ()
        fillArrayFromRAList ra.Add ral
        ra |> Seq.toList
   
   
