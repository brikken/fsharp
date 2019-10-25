module NonEmptyList

type T<'U> = NonEmptyList of 'U list

let tryCreate xs =
    match xs with
    | [] -> None
    | _ -> Some (NonEmptyList xs)

let value (NonEmptyList xs) = xs
let mapValue f (NonEmptyList xs) = f xs
let mapList f xs = NonEmptyList (mapValue f xs)
let map f xs = mapList (List.map f) xs
