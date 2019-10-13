[<AutoOpen>]
module Log

open System
open FSharp.Core.Printf

type Context =
    {
        CorrelationId : Guid
        Log : bool
    }
    static member New log : Context = { CorrelationId = Guid.NewGuid (); Log = log }

type Function<'T> = Context -> 'T

// Runs a Function<'T> with a new log context
let run log t = t (Context.New(log))

let trace v   : Function<_> = fun ctx -> if ctx.Log then printfn "CorrelationId: %A - %A" ctx.CorrelationId v
let tracef fmt              = kprintf trace fmt

let bind t uf : Function<_> = fun ctx ->
    let tv = t ctx  // Invoke t with the log context
    let u  = uf tv  // Create u function using result of t
    u ctx           // Invoke u with the log context

// >>= is the common infix operator for bind
let inline (>>=) (t, uf) = bind t uf

let r v : Function<_> = fun ctx -> v

type LogBuilder() =
    member _.Bind   (t, uf) = bind t uf
    member _.Return v       = r v
