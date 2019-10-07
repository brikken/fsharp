module TypeProvider

open FSharp.Data

type MyCsv = CsvProvider<"test.csv">

let getFirstA =
    let rows = MyCsv.Load "test.csv"
    let numrows = rows.Rows |> Seq.length
    string numrows