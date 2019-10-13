module TFIOSimple

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

let readFile = fun io path state ->
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

let lockStateFiles = fun io state ->
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

let map f =
    let mapped result =
        match result with
        | Ok (state, contents) -> Ok (state, (f contents))
        | Error error -> Error error
    mapped

let r x =
    Ok ( { log = [] }, x)

let helper x f = Result.bind f x

let map2 f =
    let mapped x =
        r (helper x f)
    mapped

let stringSomething s =
    String.filter ((>) 'd') s

let ioResult io =
    let result0 = Ok ( { log = [] }, "filewithpath.txt")
    let result05 = (map stringSomething) result0
    let result1 =
        helper result05 (fun (state, contents) ->
            let path = TFIOPath contents
            match readFile io path state with
            | Ok contents -> Ok ( { log = (ReadAction (path, contents)) :: state.log }, contents )
            | Error error -> Error error
        )
    let result2 =
        helper result1 (fun (state, contents) ->
            let path = (TFIOPath (string (System.Text.UTF8Encoding.UTF8.GetChars(contents))))
            match readFile io path state with
            | Ok contents -> Ok ( { log = (ReadAction (path, contents)) :: state.log },  contents )
            | Error error -> Error error
        )
    let result3 =
        helper result2 (fun (state, contents) ->
            let path = TFIOPath "filetowriteto.txt"
            Ok ({ log = (WriteAction (path, contents)) :: state.log }, () )
        )
    let result4 =
        helper result3 (fun (state, contents) ->
            let path = TFIOPath "filetowriteto.txt"
            match readFile io path state with
            | Ok contents -> Ok ( { log = (ReadAction (path, contents)) :: state.log }, contents)
            | Error error -> Error error
        )
    let result5 =
        helper result4 (fun (state, contents) ->
            Ok ({ log = (WriteAction (TFIOPath "filewithcopy.txt", contents)) :: state.log }, () )
        )
    result5
    // match result5 with
    // | Ok state ->
    //     match lockStateFiles io state with
    //     | Ok locks ->
    //         failwith ""
    //     | Error error -> Error error
    // | Error error -> Error error

let a = 1
