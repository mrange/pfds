namespace pfds.test

module Comparer = 

    type CollectionAbstraction<'C> = 
        {
            Empty   : 'C
            Cons    : int   -> 'C       -> 'C
            Uncons  : 'C    -> int*'C
            Lookup  : int   -> 'C       -> int
            Update  : int   -> int      -> 'C    -> 'C
            ToList  : 'C    -> int list
            ToArray : 'C    -> int []
        }
        static member New e c u l upd tl ta = 
            {
                Empty       = e
                Cons        = c
                Uncons      = u
                Lookup      = l
                Update      = upd
                ToList      = tl
                ToArray     = ta
            }

    let reference = 
        CollectionAbstraction<ReferenceImplementation.RAList<int>>.New 
            ReferenceImplementation.empty
            ReferenceImplementation.cons 
            ReferenceImplementation.uncons
            ReferenceImplementation.lookup
            ReferenceImplementation.update
            ReferenceImplementation.toList
            ReferenceImplementation.toArray

