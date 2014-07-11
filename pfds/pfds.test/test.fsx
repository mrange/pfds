
type Seq<'T> = 
    | Nil
    | Zero  of Seq<'T*'T>
    | One   of 'T*Seq<'T*'T>
