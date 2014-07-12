namespace pfds

[<StructuralEquality>]
[<NoComparison>]
exception EmptyException    

[<StructuralEquality>]
[<NoComparison>]
exception OutOfBoundsException      of int

[<StructuralEquality>]
[<NoComparison>]
exception InvariantBrokenException  of obj
