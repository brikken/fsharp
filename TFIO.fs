module TFIO

open ResultExpression

type TFIODirectory = TFIODirectory of string
type TFIOPath = TFIOPath of string
type TFIOLock = IOLock of TFIOPath
type TFIOStream = TFIOStream of int
type TFIOOffset = TFIOOffset of int64
type TFIOLength = TFIOLength of int64

type TFIOProviderError =
    | FileAlreadyExists
    | SourceFileDoesNotExist
    | DestinationFileDoesNotExist
    | IOError of string

type TFIOProvider = {
    ``open``: TFIOPath -> Result<TFIOStream,TFIOProviderError>
    close: TFIOStream -> unit
    read: TFIOStream -> Result<byte [],TFIOProviderError>
    writeAll: TFIOStream -> byte [] -> Result<unit,TFIOProviderError>
    write: TFIOStream -> byte [] -> Result<unit,TFIOProviderError>
    move: TFIOPath -> TFIOPath -> Result<unit,TFIOProviderError>
    replace: TFIOPath -> TFIOPath -> TFIOPath -> Result<unit,TFIOProviderError>
    lock: TFIOPath -> Result<TFIOLock,TFIOProviderError>
    unlock: TFIOLock -> unit
    getBackupPath: TFIOPath -> TFIOPath
    delete: TFIOPath -> Result<unit,TFIOProviderError>
    seek: TFIOStream -> TFIOOffset -> Result<unit,TFIOProviderError>
    setLength: TFIOStream -> TFIOLength -> Result<unit,TFIOProviderError>
    position: TFIOStream -> Result<TFIOOffset,TFIOProviderError>
}

type TFIOError =
    | TFIOError of string
    | TFIOProviderError of TFIOProviderError

type Action =
    | ReadAction of TFIOPath * byte []
    | WriteAction of TFIOPath * byte []

type LogItem = {
    action: Action
    rollback: unit -> Result<unit,TFIOError>
}

type Log = Action list

module Rollback =
    let inline ``open`` prov stream () = Ok (prov.close stream)
    let inline create prov path () = prov.delete path
    let inline write prov stream offset length backup () =
        result {
            do! prov.seek stream offset
            do! prov.write stream backup
            do! prov.setLength stream length
        }
    let inline writeAll prov stream () =
        result {
            do! prov.seek stream (TFIOOffset 0L)
            return! prov.setLength stream (TFIOLength 0L)
        }
    // TODO: Continue here


type TFIO<'a> = TFIO of (Log * TFIOProvider -> Log * Result<'a,TFIOError>)

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
let rollback : Log -> Result<unit,'b> = fun state ->
    Ok ()

let readFile : TFIOProvider -> TFIOPath -> Log -> Result<byte [],TFIOProviderError> = fun io path state ->
    let prevWriteContents logItem =
        match logItem with
        | WriteAction (writePath, contents) when writePath = path -> Some contents
        | _ -> None
    match state |> List.rev |> List.tryPick prevWriteContents with
    | Some contents -> Ok contents
    | None ->
        result {
            let! stream = io.``open`` path
            let! contents = io.read stream
            do io.close stream
            return contents
        }

let readFileTFIO = fun path ->
    let inner (state, prov) =
        match readFile prov path state with
        | Ok contents -> (ReadAction (path, contents)::state, Ok contents )
        | Error e -> (state, Error e |> Result.mapError TFIOProviderError)
    TFIO inner

// TODO: This needs to be more robust, ie. ensure no invalid filenames are returned
let getTempPath : TFIOPath -> System.Guid -> TFIOPath = fun path guid ->
    let (TFIOPath path') = path
    TFIOPath (path' + "." + (sprintf "%8s" (guid.ToString())))

let getTempFileStream : TFIOProvider -> TFIOPath -> Result<TFIOPath * TFIOStream,TFIOProviderError> = fun prov path ->
    let rec getTempFileStreamRec () =
        let pathTemp = getTempPath path (System.Guid.NewGuid())
        match prov.``open`` pathTemp with
        | Ok stream -> Ok (pathTemp, stream)
        | Error FileAlreadyExists -> getTempFileStreamRec ()
        | Error e -> Error e
    getTempFileStreamRec ()

let moveOrReplace : TFIOProvider -> TFIOPath -> TFIOPath -> TFIOPath -> Result<unit,TFIOProviderError> = fun prov src dst bkp ->
    let rec moveOrReplaceRec () =
        match prov.move src dst with
        | Ok _ -> Ok ()
        | Error FileAlreadyExists ->
            match prov.replace src dst bkp with
            | Ok _ -> Ok ()
            | Error DestinationFileDoesNotExist -> moveOrReplaceRec ()
            | Error e -> Error e
        | Error e -> Error e
    moveOrReplaceRec ()

// TODO: This comp. expr. should keep a log of what's been done, such that it can roll it back. This is a mini TFIO-expr. in itself
let writeFile : TFIOProvider -> TFIOPath -> byte [] -> Result<unit,TFIOProviderError> = fun prov path contents ->
    result {
        let! (pathTemp, stream) = getTempFileStream prov path
        do! prov.writeAll stream contents
        do prov.close stream
        do! moveOrReplace prov pathTemp path (prov.getBackupPath path)
    }

let writeFileTFIO = fun path contents ->
    let inner (state, prov) =
        (WriteAction (path, contents)::state, writeFile prov path contents |> Result.mapError TFIOProviderError)
    TFIO inner

// TODO: When more actions are added, this needs to carry a state, to keep track of already locked files, moved files, deleted files, etc.
let fileToLock a =
    match a with
    | ReadAction (path,_) -> [ path ]
    | WriteAction (path,_) -> [ path ]

let lockLogFiles : TFIOProvider -> Log -> Result<TFIOLock list,TFIOProviderError> = fun io state ->
    let lockFileFolder s t =
        match s with
        | Ok locks ->
            match io.lock t with
            | Ok lock -> Ok (lock::locks)
            | Error e ->
                locks |> List.map io.unlock |> ignore
                Error e
        | Error e -> Error e
    state
    |> List.collect fileToLock
    |> List.distinct
    |> List.fold lockFileFolder (Ok [])
    |> Result.map List.rev

// TODO: This can return 3 things: Validation success, validation fail, IO exception. The Result type is not sufficient
let validateLogActions : TFIOProvider -> Log -> Result<unit,string> = fun io state ->
    let validateLogAction action =
        match action with
        | ReadAction (path, contents) -> failwith "Not Implemented"
        | WriteAction (path, contents) -> failwith "Not Implemented"
    failwith ""

let atomically : TFIOProvider -> TFIO<'b> -> Log * Result<'b,TFIOError> = fun prov tfio ->
    let (TFIO f) = tfio
    f ([], prov)