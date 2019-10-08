module State

type State<'a,'b> = State of 'a * 'b

type StateBuilder() =
    member __.Bind(State (a, b), f) =
        let (State (a', b')) = f b
        State (a @ a', b')
    member __.Return(x) =
        State ([], x)

let state = StateBuilder()

type ResultState<'TState,'T,'TError> = ResultState of 'TState * Result<'T,'TError>
type Atomic<'T,'TError> =
    | All of 'T
    | None of 'TError

type ResultStateBuilder() =
    member __.Bind(x, f) =
        let (ResultState (state, result)) = x
        match result with
        | Error _ -> x
        | Ok value ->
            let (ResultState (state',result')) = f value
            ResultState (state @ state', result')
    member __.Return(x) =
        ResultState ([], Ok x)

let resultstate = ResultStateBuilder()

let inline atomic rollback (ResultState (state, result)) =
    match result with
    | Ok x -> Ok (All x)
    | Error err ->
        match rollback state with
        | Ok _ -> Ok (None err)
        | Error err -> Error err