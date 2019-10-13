// Learn more about F# at http://fsharp.org

open System
open State


[<EntryPoint>]
let main argv =
    let dummy =
        result {
            //let! a = Ok 1
            let! a = Error 1
            let b = a + 1
            let! c = Error b
            return c
        }
    match dummy with
    | Ok a -> printfn "Ok %i" a
    | Error b -> printfn "Error %i" b
    0 // return an integer exit code
