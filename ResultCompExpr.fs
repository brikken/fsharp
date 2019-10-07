module ResultTest

type Success =
    | SA
    | SB
    | SC

type Failure =
    | FA
    | FB

let test a : Result<Success,Failure> =
    Ok SA
