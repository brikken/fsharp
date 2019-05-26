open System

type FlashRow =
    | Position of Position
    | Trade of Trade

and Position =
    | FX of FXPosition
    | Security of SecurityPosition

and FXPosition = {
    ccyBought: Currency
    ccySold: Currency
    amount: decimal
}

and Currency = Currency of string

and SecurityPosition = {
    isin: ISIN
    amount: int
}

and ISIN = ISIN of string

and Trade =
    | Swap of SwapTrade
    | Deposit of DepositTrade
    | Camp of CampTrade

and SwapTrade = {
    leg1: SwapLeg
    leg2: SwapLeg
}

and SwapLeg = {
    currency: Currency
    notional: decimal
}

and DepositTrade = {
    currency: Currency
    notional: decimal
    rate: decimal
    start: DateTime
    maturity: DateTime
}

and CampTrade = {
    currency: Currency
    notional: decimal
    rate: decimal
    start: DateTime
    maturity: DateTime
}

let row = Position (FX {ccyBought = Currency "EUR"; ccySold = Currency "GBP"; amount = 150000M})
let row2 = Position (Security { isin = ISIN "DK123"; amount = 2500 })
let row3 = Trade (Swap { leg1 = { currency = Currency "GBP"; notional = 2500M }; leg2 = { currency = Currency "USD"; notional = 3500M } } )

let getNotional row =
    match row with
        | Position (FX fxpos) -> Some [ (fxpos.ccyBought, fxpos.amount); (fxpos.ccySold, -fxpos.amount) ]
        | Position (Security _) -> None
        | Trade (Swap st) -> Some [ (st.leg1.currency, st.leg1.notional); (st.leg2.currency, -st.leg2.notional) ]
        | Trade (Deposit dt) -> Some [ (dt.currency, dt.notional) ]
        | Trade (Camp ct) -> Some [ (ct.currency, ct.notional) ]

let notionals = List.collect (getNotional >> Option.defaultValue []) [ row; row2; row3 ]

notionals
|> List.groupBy (fun (Currency ccy, _) -> ccy)
|> List.map (fun (ccy, xs) -> (ccy, List.sumBy (fun (_, n) -> n) xs))
