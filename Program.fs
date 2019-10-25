// Learn more about F# at http://fsharp.org

open NonEmptyList

[<EntryPoint>]
let main _ =
    //let invalid = T.NonEmptyList [ 1; 2; 3; ]
    match NonEmptyList.tryCreate [ 1; 2; 3; ] with
    | Some l ->
        let sum = l |> NonEmptyList.map ((+) 1) |> NonEmptyList.mapList (List.filter ((<) 2)) |> NonEmptyList.mapValue (List.sum)
        printfn "Sum of list 1, 2, 3 with 1 added and filtered above 2 is %i" sum
    | None -> printfn "Not a non-empty list"
    0 // return an integer exit code
