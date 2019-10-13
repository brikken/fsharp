// Learn more about F# at http://fsharp.org

open System
open State

let init = Ok 1

let dummy () : Result<int,int> =
    printfn "bind begginning"
    match init with
    | Error error ->
        printfn "bind ending in error"
        Error error
    | Ok a ->
        printfn "We passed the init!"
        let b = a + 1
        printfn "b is %i" b
        let r1 =
            printfn "bind beginning"
            match Error b with
            | Error error ->
                printfn "bind ending in error"
                Error error
            | Ok c ->
                printfn "We passed the C!"
                printfn "return"
                let r2 = Ok c
                printf "ok bind ending in "
                match r2 with
                | Ok _ -> printfn "ok"
                | Error _ -> printfn "error"
                r2
        printf "ok bind ending in "
        match r1 with
        | Ok _ -> printfn "ok"
        | Error _ -> printfn "error"
        r1

[<EntryPoint>]
let main argv =
    match dummy () with
    | Ok a -> printfn "Ok %i" a
    | Error b -> printfn "Error %i" b
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
            return (
                printfn "We passed the C!"
                c
            )
        }
    match dummy with
    | Ok a -> printfn "Ok %i" a
    | Error b -> printfn "Error %i" b
    0 // return an integer exit code
