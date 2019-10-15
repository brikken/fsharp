module TFIO

type TFIOPath = TFIOPath of string

type TFIOLock = IOLock of TFIOPath

type TFIOProvider = {
    read: TFIOPath -> byte []
    write: (TFIOPath * byte []) -> unit
    lock: TFIOPath -> TFIOLock
    unlock: TFIOLock -> unit
}

type Action =
    | ReadAction of TFIOPath * byte []
    | WriteAction of TFIOPath * byte []

type State = Action list

type TFIOError = string

type TFIO<'a> = TFIO of (State * TFIOProvider -> State * Result<'a,TFIOError>)

type TFIOBuilder() =
    member _.Return(x) =
        let inner (state, _) =
            (state, x)
        TFIO inner
    member __.Bind(x, f) =
        let inner (state, prov) =
            let (TFIO fx) = x
            let (state', x') = fx (state, prov)
            match x' with
            | Ok a ->
                let (TFIO f') = f a
                f' (state', prov)
            | Error e ->
                (state', Error e)
        TFIO inner

let tfio = TFIOBuilder

// TODO: This signature is probably not correct
let rollback : State -> Result<unit,'b> = fun state ->
    Ok ()

let readFile : TFIOProvider -> TFIOPath -> State -> Result<byte [],string> = fun io path state ->
    let prevWriteContents logItem =
        match logItem with
        | WriteAction (writePath, contents) when writePath = path -> Some contents
        | _ -> None
    match state |> List.rev |> List.tryPick prevWriteContents with
    | Some contents -> Ok contents
    | None ->
        try
            Ok (io.read path)
        with
            ex -> Error ex.Message

let readFileTFIO = fun path ->
    let inner (state, prov) =
        match readFile prov path state with
        | Ok contents -> (ReadAction (path, contents)::state, Ok contents )
        | Error e -> (state, Error e)
    TFIO inner

let writeFileTFIO = fun path contents ->
    let inner (state, _) =
        (WriteAction (path, contents)::state, Ok () )
    TFIO inner

let lockStateFiles : TFIOProvider -> State -> Result<TFIOLock list,string> = fun io state ->
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
    state
    |> List.collect fileToLock
    |> List.fold lockFileFolder (Ok [])
    |> Result.map List.rev

// TODO: This can return 3 things: Validation success, validation fail, IO exception. The Result type is not sufficient
let validateLogActions : TFIOProvider -> State -> Result<unit,string> = fun io state ->
    let validateLogAction action =
        match action with
        | ReadAction (path, contents) -> failwith "Not Implemented"
        | WriteAction (path, contents) -> failwith "Not Implemented"
    failwith ""

let atomically : TFIOProvider -> TFIO<'b> -> State * Result<'b,TFIOError> = fun prov tfio ->
    let (TFIO f) = tfio
    f ([], prov)