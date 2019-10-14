// Learn more about F# at http://fsharp.org

let getValueM n =
    let inner state =
        (state, n)
    M inner

[<EntryPoint>]
let main argv =
    let m =
        state {
            let! a = getValueM 1
            return a
        }
    let m2 =
        state {
            let! b = m
            return b
        }
    let (M f) = m2
    let (State s, v) = f (State 0)
    printfn "State %i, value %i" s v
    0 // return an integer exit code
