module Logic3

open System
open Input3
open FSharp.Collections
open System

let tee f x =
  f x
  x

// Some notes:
// For a 2D array, length1 is the number of rows
// length2 is the number of columns
let problem3a =
  let isSymbol c =
    '.' :: [ '0' .. '9' ] |> List.contains c |> not

  let checkAround (arr:char array2d) (r, c) =
    let hasSymbolAbove arr (r, c) =
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

    let hasSymbolLeft arr (r, c) =
      if c = 0 then false else arr[r, c - 1] |> isSymbol

    let hasSymbolRight arr (r, c) =
      if c = (Array2D.length2 arr - 1) then
        false
      else
        arr[r, c + 1] |> isSymbol

    let hasSymbolBelow arr (r, c) =
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

        bl || above || br

    (hasSymbolAbove arr (r, c))
    || (hasSymbolBelow arr (r, c))
    || (hasSymbolLeft arr (r, c))
    || (hasSymbolRight arr (r, c))

  failwith "Not implemented"
