module Input3

open FSharp.Collections
open System
open Utility

let input3a1Raw =
  """
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
"""

let input3a (inp: string) =
  let ses =
    inp.Split Environment.NewLine |> Array.filter (String.IsNullOrWhiteSpace >> not)

  let rows = Array.length ses

  let cols = ses |> Array.map String.length |> Array.max

  let arr = Array2D.create rows cols '.'

  for r in 0 .. (Array2D.length1 arr - 1) do
    let sr = ses[r] |> seq |> Array.ofSeq |> Array.filter (Char.IsWhiteSpace >> not)

    for c in 0 .. (Array2D.length2 arr - 1) do
      arr[r, c] <- sr[c]

  tee (printfn "%A") arr
