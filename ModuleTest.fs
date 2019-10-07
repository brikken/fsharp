module ModuleTest

module HiddenType =
    type HiddenType<'a> = private HiddenType of 'a

    let create x = HiddenType x
    let value (HiddenType x) = x

open HiddenType

let hidden = HiddenType.create 10
let hiddenValue = HiddenType.value hidden
//let hiddenDirect = HiddenType 10 // won't compile
