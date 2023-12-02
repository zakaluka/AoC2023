module Logic1

open Input1

let problem () =
  input.Split '\n'
  |> List.iter (fun line -> printfn "%s" line)
