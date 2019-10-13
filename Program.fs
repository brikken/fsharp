// Learn more about F# at http://fsharp.org

open System
open State

let init = Ok 1

let dummy () : Result<int,int> =
    match init with
    | Error error -> Error error
    | Ok a ->
        printfn "We passed the init!"
        let b = a + 1
        printfn "b is %i" b
        match Error b with
        | Error error -> Error error
        | Ok c ->
            printfn "We passed the C!"
            Ok c

[<EntryPoint>]
let main argv =
    // match dummy () with
    // | Ok a -> printfn "Ok %i" a
    // | Error b -> printfn "Error %i" b
    let dummy =
        result {
            let! a = init
            let! c =
                Error (
                    printfn "We passed the init!"
                    let b = a + 1
                    printfn "b is %i" b
                    b
                )
            return! (
                printfn "We passed the C!"
                c
            )
        }
    match dummy with
    | Ok a -> printfn "Ok %i" a
    | Error b -> printfn "Error %i" b
    0 // return an integer exit code
