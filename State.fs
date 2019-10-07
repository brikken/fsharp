module State

type State<'a,'b> = State of 'a * 'b

type StateBuilder() =
    member __.Bind(State (a, b), f) =
        let (State (a', b')) = f b
        State (a @ a', b')
    member __.Return(x) =
        State ([], x)

let state = StateBuilder()
