type OptionBuilder() =
    member this.Bind(x, f) = Option.bind f x
    member this.Return(x) = Some x
    member this.ReturnFrom(x) = x

let option = OptionBuilder()

let myVal : int option = option {
    let! first = Some 10
    let! second = Some (first * 10)
    let! third = None
    let! fourth = Some (third * 10)
    return! Some fourth
}

let (>>=) x f = Option.bind f x

Some 10 >>= (fun x -> Some (x * 10)) >>= (fun x -> Some (x * 10))
