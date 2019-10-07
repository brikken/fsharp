// Learn more about F# at http://fsharp.org

open System
open TypeProvider
open ListExtensions
open BenchmarkDotNet.Attributes
open Infix

[<EntryPoint>]
let main argv =
    printfn "%i" c
    0 // return an integer exit code
