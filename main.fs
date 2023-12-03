open System
open Input2

[<EntryPoint>]
let main argv =
  Logic2.problemb
    Input2.inputa2
    { MaxRed = 12
      MaxGreen = 13
      MaxBlue = 14 }
  |> printfn "%d"

  0 // return an integer exit code
