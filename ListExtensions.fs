module ListExtensions

module DistinctList =
    type DistinctList<'a> = private DistinctList of 'a list

    let create xs =
        if List.length (List.distinct xs) = List.length xs then Some (DistinctList xs) else None

    let value (DistinctList xs) = xs    

open DistinctList

module SortedList =
    type SortedList<'a> = private SortedList of 'a list

    let create xs = List.sort xs
    let value (SortedList xs) = xs

open SortedList

let distinctListTest = DistinctList.create [1; 2; 3]
let sortedList = SortedList.create [1; 3; 2]

type MaybeBuilder() =
    member __.Bind(x, f) =
        match x with
        | None -> None
        | Some y -> f y
    member __.Return(x) =
        Some x

let maybe = MaybeBuilder()

let distinct xs =
    let rec distinct xs seen =
        match (xs, seen) with
        | ([], _) -> true
        | ([_], _) -> true
        | ([x; y], _) -> x <> y
        | (x::xs, []) -> distinct xs [x]
        | (x::xs, seen) ->
            if List.contains x seen
            then false
            else distinct xs (x::seen)
    distinct xs []    

let isDistinct = distinct [1 .. 10000]

let distinctMutable xs =
    let mutable seen = []
    let rec distinct xs =
        match (xs, seen) with
        | ([], _) -> true
        | ([_], _) -> true
        | ([x; y], _) -> x <> y
        | (x::xs, s) ->
            if List.contains x seen
            then false
            else
                seen <- x::s
                distinct xs
    distinct xs

let isStillDistinct = distinctMutable [1 .. 100000]
