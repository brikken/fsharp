type OrgId =
    | OrgId of string * OrgId list
    | Rn of string

let konci =
    OrgId ("KONCI", [
        Rn "3628"
        OrgId ("W34CI", [
             Rn "3001"
             Rn "3847"
             ] )
        ] )

let rec rns orgId =
    match orgId with
    | Rn rn -> [ rn ]
    | OrgId (_, orgIds) -> List.collect rns orgIds

let konciRns = rns konci
