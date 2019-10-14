[<AutoOpen>]
module State

type State<'a> = State of 'a

type M<'a,'b> = M of (State<'a> -> State<'a> * 'b)

type StateBuilder() =
    member _.Bind(x, f) =
        let inner state =
            let (M fi) = x
            let (sn, vn) = fi state
            let (M fn) = f vn
            fn sn
        M inner
    member _.Return(x) =
        let inner state =
            (state, x)
        M inner

let state = StateBuilder()
