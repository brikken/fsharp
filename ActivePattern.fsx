let (|Even|Odd|Complex|) (real, imaginary) =
    if imaginary <> 0
        then Complex "It's complex"
        else if real % 2 = 0
            then Even "It's even"
            else Odd "It's odd"

let numType (real, img) =
    match (real, img) with
    | Even s -> printfn "%s" s //"%i is even" real
    | Odd _ -> printfn "%i is odd" real
    | Complex s -> printfn "%i + %ii is a complex number" real img

numType (10, 0)
numType (11, 0)
numType (12, 2)

open System.Drawing

let (|RGB|) (col : System.Drawing.Color) =
     ( col.R, col.G, col.B )

let (|HSB|) (col : System.Drawing.Color) =
   ( col.GetHue(), col.GetSaturation(), col.GetBrightness() )

let printRGB (col: System.Drawing.Color) =
   match col with
   | RGB(r, g, b) -> printfn " Red: %d Green: %d Blue: %d" r g b

let printHSB (col: System.Drawing.Color) =
   match col with
   | HSB(h, s, b) -> printfn " Hue: %f Saturation: %f Brightness: %f" h s b

let printAll col colorString =
  printfn "%s" colorString
  printRGB col
  printHSB col

printAll Color.Red "Red"
printAll Color.Black "Black"
printAll Color.White "White"
printAll Color.Gray "Gray"
printAll Color.BlanchedAlmond "BlanchedAlmond"

let err = 1.e-10

let isNearlyIntegral (x:float) = abs (x - round(x)) < err

let (|Square|_|) (x : int) =
  if isNearlyIntegral (sqrt (float x)) then Some(x)
  else None

let (|Cube|_|) (x : int) =
  if isNearlyIntegral ((float x) ** ( 1.0 / 3.0)) then Some(x)
  else None

let examineNumber x =
   match x with
      | Cube x -> printfn "%d is a cube" x
      | _ -> ()
   match x with
      | Square x -> printfn "%d is a square" x
      | _ -> ()

let findSquareCubes x =
   match x with
       | Cube x & Square _ -> printfn "%d is a cube and a square" x
       | Cube x -> printfn "%d is a cube" x
       | Square x -> printfn "%d is a square" x
       | _ -> ()
         

[ 1 .. 1000 ] |> List.iter findSquareCubes
