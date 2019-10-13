// Learn more about F# at http://fsharp.org

let log = LogBuilder ()

let f x y =
    log {
        do! tracef "f: called with: x = %d, y = %d" x y
        return x + y
    }

let g =
    log {
        do! trace "g: starting..."
        let! v = f 1 2
        do! tracef "g: f produced %d" v
        return v
    }

[<EntryPoint>]
let main _ =
    printfn "g produced %A" (run false g)
    0 // return an integer exit code
