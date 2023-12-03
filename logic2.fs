module Logic2

open System
open Input2

type Game2a =
  { Id: int
    MaxRed: int
    MaxGreen: int
    MaxBlue: int }

type Game2aLimit =
  { MaxRed: int
    MaxGreen: int
    MaxBlue: int }

let problema (input: string) (limit: Game2aLimit) =
  let games =
    input.Split [| '\n' |] |> Array.filter (fun l -> String.IsNullOrEmpty(l) |> not)

  let mutable gamesParsed: Game2a list = []

  for game in games do
    let level1 =
      game.Split [| ':' |] |> Array.filter (fun l -> String.IsNullOrEmpty(l) |> not)

    let level1Left =
      level1[0].Split [| ' ' |]
      |> Array.filter (fun l -> String.IsNullOrEmpty(l) |> not)

    let gameNum = level1Left[1] |> int

    let level2 =
      level1[1].Split [| ';'; ',' |]
      |> Array.filter (fun l -> String.IsNullOrEmpty(l) |> not)

    let mutable maxRed = 0
    let mutable maxGreen = 0
    let mutable maxBlue = 0

    for l2 in level2 do
      let l2parsed =
        l2.Split [| ' ' |] |> Array.filter (fun l -> String.IsNullOrEmpty(l) |> not)

      if
        l2parsed[1].Equals("red", StringComparison.InvariantCultureIgnoreCase)
        && (int l2parsed[0]) > maxRed
      then
        maxRed <- int l2parsed[0]
      else if
        l2parsed[1].Equals("green", StringComparison.InvariantCultureIgnoreCase)
        && (int l2parsed[0]) > maxGreen
      then
        maxGreen <- int l2parsed[0]
      else if
        l2parsed[1].Equals("blue", StringComparison.InvariantCultureIgnoreCase)
        && (int l2parsed[0]) > maxBlue
      then
        maxBlue <- int l2parsed[0]

    gamesParsed <-
      { Id = gameNum
        MaxRed = maxRed
        MaxGreen = maxGreen
        MaxBlue = maxBlue }
      :: gamesParsed

  gamesParsed
  |> List.filter (fun g ->
    g.MaxRed <= limit.MaxRed
    && g.MaxGreen <= limit.MaxGreen
    && g.MaxBlue <= limit.MaxBlue)
  |> List.fold (fun acc e -> acc + e.Id) 0

let problemb (input: string) (limit: Game2aLimit) =
  let games =
    input.Split [| '\n' |] |> Array.filter (fun l -> String.IsNullOrEmpty(l) |> not)

  let mutable gamesParsed: Game2a list = []

  for game in games do
    let level1 =
      game.Split [| ':' |] |> Array.filter (fun l -> String.IsNullOrEmpty(l) |> not)

    let level1Left =
      level1[0].Split [| ' ' |]
      |> Array.filter (fun l -> String.IsNullOrEmpty(l) |> not)

    let gameNum = level1Left[1] |> int

    let level2 =
      level1[1].Split [| ';'; ',' |]
      |> Array.filter (fun l -> String.IsNullOrEmpty(l) |> not)

    let mutable maxRed = 0
    let mutable maxGreen = 0
    let mutable maxBlue = 0

    for l2 in level2 do
      let l2parsed =
        l2.Split [| ' ' |] |> Array.filter (fun l -> String.IsNullOrEmpty(l) |> not)

      if
        l2parsed[1].Equals("red", StringComparison.InvariantCultureIgnoreCase)
        && (int l2parsed[0]) > maxRed
      then
        maxRed <- int l2parsed[0]
      else if
        l2parsed[1].Equals("green", StringComparison.InvariantCultureIgnoreCase)
        && (int l2parsed[0]) > maxGreen
      then
        maxGreen <- int l2parsed[0]
      else if
        l2parsed[1].Equals("blue", StringComparison.InvariantCultureIgnoreCase)
        && (int l2parsed[0]) > maxBlue
      then
        maxBlue <- int l2parsed[0]

    gamesParsed <-
      { Id = gameNum
        MaxRed = maxRed
        MaxGreen = maxGreen
        MaxBlue = maxBlue }
      :: gamesParsed

  gamesParsed
  |> List.map (fun g -> g.MaxRed * g.MaxGreen * g.MaxBlue)
  |> List.fold (fun acc e -> acc + e) 0
