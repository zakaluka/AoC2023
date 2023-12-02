module Logic1

open System
open Input1

let tee f x =
  f x
  x

let problema () =
  inputa.Split [| '\n' |]
  |> Array.filter (fun l -> String.IsNullOrEmpty(l) |> not)
  |> Array.Parallel.map (fun line ->
    sprintf
      "%c%c"
      (line
       |> Seq.find (fun c -> List.contains c [ '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' ]))
      (line
       |> Seq.findBack (fun c -> List.contains c [ '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' ]))
    |> int)
  |> Array.fold (+) 0

let problemb () =
  let strings =
    inputb.Split [| '\n' |]
    |> Array.filter (fun l -> String.IsNullOrEmpty(l) |> not)

  let minFind cur curi num numi =
    if numi >= 0 && numi < curi then
      (num, numi)
    else
      (cur, curi)

  let maxFind cur curi num numi =
    if numi >= 0 && numi > curi then
      (num, numi)
    else
      (cur, curi)

  let mutable acc = 0

  for line in strings do
    printfn "line: %s" line

    let first =
      Seq.tryFind (fun c -> List.contains c [ '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' ]) line
      |> fun x -> if Option.isSome x then x.Value |> string |> int else -1

    let firstIndex =
      Seq.tryFindIndex (fun c -> List.contains c [ '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' ]) line
      |> Option.defaultValue -1


    printfn "cc: %A" (first, firstIndex)

    let (first, firstIndex) =
      minFind first firstIndex 1 (line.IndexOf("one")) |> tee (printfn "%A")

    let (first, firstIndex) =
      minFind first firstIndex 2 (line.IndexOf("two")) |> tee (printfn "%A")

    let (first, firstIndex) =
      minFind first firstIndex 3 (line.IndexOf("three")) |> tee (printfn "%A")

    let (first, firstIndex) =
      minFind first firstIndex 4 (line.IndexOf("four")) |> tee (printfn "%A")

    let (first, firstIndex) =
      minFind first firstIndex 5 (line.IndexOf("five")) |> tee (printfn "%A")

    let (first, firstIndex) =
      minFind first firstIndex 6 (line.IndexOf("six")) |> tee (printfn "%A")

    let (first, firstIndex) =
      minFind first firstIndex 7 (line.IndexOf("seven")) |> tee (printfn "%A")

    let (first, firstIndex) =
      minFind first firstIndex 8 (line.IndexOf("eight")) |> tee (printfn "%A")

    let (first, firstIndex) =
      minFind first firstIndex 9 (line.IndexOf("nine")) |> tee (printfn "%A")

    let last =
      Seq.tryFindBack (fun c -> List.contains c [ '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' ]) line
      |> fun x -> if Option.isSome x then x.Value |> string |> int else -1

    let lastIndex =
      Seq.tryFindIndexBack (fun c -> List.contains c [ '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' ]) line
      |> Option.defaultValue -1

    let (last, lastIndex) = maxFind last lastIndex 1 (line.LastIndexOf("one"))
    let (last, lastIndex) = maxFind last lastIndex 2 (line.LastIndexOf("two"))
    let (last, lastIndex) = maxFind last lastIndex 3 (line.LastIndexOf("three"))
    let (last, lastIndex) = maxFind last lastIndex 4 (line.LastIndexOf("four"))
    let (last, lastIndex) = maxFind last lastIndex 5 (line.LastIndexOf("five"))
    let (last, lastIndex) = maxFind last lastIndex 6 (line.LastIndexOf("six"))
    let (last, lastIndex) = maxFind last lastIndex 7 (line.LastIndexOf("seven"))
    let (last, lastIndex) = maxFind last lastIndex 8 (line.LastIndexOf("eight"))
    let (last, lastIndex) = maxFind last lastIndex 9 (line.LastIndexOf("nine"))

    acc <-
      (first |> tee (printf "first: %A, ")) * 10
      + (last |> tee (printfn "last: %A"))
      + acc

  acc
