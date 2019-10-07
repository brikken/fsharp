// Learn more about F# at http://fsharp.org

open System
open State

type Action =
    | DoStuff
    | DoMoreStuff

let doStuff s =
    State ([ DoStuff ], s + 1)

let doMoreStuff s =
    State ([ DoMoreStuff ], s - 1)

let getUnionCaseName (x:'a) = 
    match Reflection.FSharpValue.GetUnionFields(x, typeof<'a>) with
    | case, _ -> case.Name  

[<EntryPoint>]
let main argv =
    let valueState =
        state {
            let! two = doStuff 1
            let! one = doMoreStuff two
            return one
        }
    let (State (actions, value)) = valueState
    actions |> List.iter (getUnionCaseName >> printfn "%s")
    printfn "%i" value
    0 // return an integer exit code
