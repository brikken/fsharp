module NonEmptyList

type T<'U>

val tryCreate : 'U list -> T<'U> option
val value : T<'U> -> 'U list
val mapValue : ('U list -> 'V) -> T<'U> -> 'V
val mapList : ('U list -> 'V list) -> T<'U> -> T<'V>
val map : ('U -> 'V) -> T<'U> -> T<'V>
