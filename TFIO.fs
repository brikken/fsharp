module TFIO

type TFIOPath = TFIOPath of string

type TFIOLock<'a> = IOLock of 'a

type TFIOProvider<'a> = {
    read: TFIOPath -> byte []
    write: (TFIOPath * byte []) -> unit
    lock: TFIOPath -> TFIOLock<'a>
    unlock: TFIOLock<'a> -> unit
}

type Action =
    | ReadAction of TFIOPath * byte []
    | WriteAction of TFIOPath * byte []

type ActionResult =
    | ReadResult of byte []
    | WriteResult

type State = {
    log: Action list
}

type TFIO<'a> = TFIO of (State -> Result<State * 'a,string>)

type TFIOBuilder() =
    member __.Return(x) =
        let inner state =
            Ok (state, x)
        TFIO inner
    member __.Bind(x, f) =
        // TODO: Is this right? Will remaining expressions slide through, once an error has happened?
        let bound state =
            let (TFIO fPrev) = x
            match fPrev state with
            | Ok (stateNew, y) -> 
                let (TFIO fNew) = f y
                fNew stateNew
            | Error error -> Error error
        TFIO bound

let tfio = TFIOBuilder

let rollback : State -> Result<unit,'b> = fun state ->
    Ok ()

let readFile : TFIOProvider<'a> -> TFIOPath -> State -> Result<byte [],string> = fun io path state ->
    let prevWriteContents logItem =
        match logItem with
        | WriteAction (writePath, contents) when writePath = path -> Some contents
        | _ -> None
    match state.log |> List.rev |> List.tryPick prevWriteContents with
    | Some contents -> Ok contents
    | None ->
        try
            Ok (io.read path)
        with
            ex -> Error ex.Message

let readFileTFIO = fun io path ->
    let inner state =
        match readFile io path state with
        | Ok contents -> Ok ( { log = (ReadAction (path, contents)) :: state.log }, ReadResult contents )
        | Error error -> Error error
    TFIO inner

let writeFileTFIO = fun path contents ->
    let inner = fun state ->
        Ok ( { log = (WriteAction (path, contents)) :: state.log }, WriteResult )
    TFIO inner

let lockStateFiles : TFIOProvider<'a> -> State -> Result<TFIOLock<'a> list,string> = fun io state ->
    // TODO: When more actions are added, this needs to carry a state, to keep track of already locked files, moved files, deleted files, etc.
    let fileToLock a =
        match a with
        | ReadAction (path,_) -> [ path ]
        | WriteAction (path,_) -> [ path ]
    let lockFileFolder s t =
        match s with
        | Ok locks ->
            try
                Ok ((io.lock t)::locks)
            with
                ex ->
                    locks |> List.map io.unlock |> ignore
                    Error ex.Message
        | _ -> s
    state.log
    |> List.collect fileToLock
    |> List.fold lockFileFolder (Ok [])
    |> Result.map List.rev

// TODO: This can return 3 things: Validation success, validation fail, IO exception. The Result type is not sufficient
let validateLogActions : TFIOProvider<'a> -> State -> Result<unit,string> = fun io state ->
    let validateLogAction action =
        match action with
        | ReadAction (path, contents) -> failwith ""
    failwith ""

let atomically : TFIOProvider<'a> -> TFIO<ActionResult> -> Result<ActionResult,string> = fun io tfio ->
    let (TFIO action) = tfio
    let result = action { log = [] }
    match result with
    | Ok (state, actionResult) ->
        match lockStateFiles io state with
        | Ok locks ->
            // TODO: call validateLogActions
            failwith ""
        | Error error -> Error error
    | Error error -> Error error
