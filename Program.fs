// Learn more about F# at http://fsharp.org

open System
open State

type Action =
    | DoStuff
    | DoMoreStuff

let doStuff s =
    if s > 1 then
        ResultState ([ DoStuff ], Ok (s + 1))
    else
        ResultState ([], Error "Too small")

let doMoreStuff s =
    if s < 1 then
        ResultState ([ DoMoreStuff ], Ok (s - 1))
    else
        ResultState ([], Error "Too large")

let getUnionCaseName (x:'a) = 
    match Reflection.FSharpValue.GetUnionFields(x, typeof<'a>) with
    | case, _ -> case.Name  

let testRollback state : Result<unit,string> =
    Ok ()

[<EntryPoint>]
let main argv =
    let valueState =
        resultstate {
            let! three = doStuff 2
            let zero = three - 3
            let! negativeOne = doMoreStuff zero
            //let! zero = doStuff negativeOne
            return zero
        }
        |> (atomic testRollback) 
    match valueState with
    | Ok atomic ->
        match atomic with
        | All x -> printfn "Success: %i" x
        | None err -> printfn "Rolled back: %s" err
    | Error err -> printfn "Error: %s" err
    0 // return an integer exit code
