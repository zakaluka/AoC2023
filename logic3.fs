module Logic3

open Utility
open FSharp.Collections

// Some notes:
// For a 2D array, length1 is the number of rows
// length2 is the number of columns
let problem3a (arr: char array2d) =
  let isNumber c = [ '0' .. '9' ] |> List.contains c

  let isSymbol c =
    '.' :: [ '0' .. '9' ] |> List.contains c |> not

  let findNumbers (arr:char array2d) =
    let mutable 

  let hasSymbolAround (arr: char array2d) (r, c) =
    let hasSymbolAbove (arr: char array2d) (r, c) =
      if r = 0 then
        false
      else
        let above = arr[r - 1, c] |> isSymbol
        let tl = if c = 0 then false else (arr[r - 1, c - 1] |> isSymbol)

        let tr =
          if c = ((Array2D.length2 arr) - 1) then
            false
          else
            (arr[r - 1, c + 1] |> isSymbol)

        tl || above || tr

    let hasSymbolLeft (arr: char array2d) (r, c) =
      if c = 0 then false else arr[r, c - 1] |> isSymbol

    let hasSymbolRight (arr: char array2d) (r, c) =
      if c = (Array2D.length2 arr - 1) then
        false
      else
        arr[r, c + 1] |> isSymbol

    let hasSymbolBelow (arr: char array2d) (r, c) =
      if r = (Array2D.length1 arr - 1) then
        false
      else
        let below = arr[r + 1, c] |> isSymbol
        let bl = if c = 0 then false else (arr[r + 1, c - 1] |> isSymbol)

        let br =
          if c = (Array2D.length2 arr - 1) then
            false
          else
            arr[r + 1, c + 1] |> isSymbol

        bl || below || br

    (hasSymbolAbove arr (r, c))
    || (hasSymbolBelow arr (r, c))
    || (hasSymbolLeft arr (r, c))
    || (hasSymbolRight arr (r, c))

  printfn "(0,0) %A" (hasSymbolAround arr (0, 0))
  printfn "(0,1) %A" (hasSymbolAround arr (0, 1))
  printfn "(0,2) %A" (hasSymbolAround arr (0, 2))
  printfn "(0,3) %A" (hasSymbolAround arr (0, 3))
