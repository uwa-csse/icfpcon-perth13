
let eval () =
    // eval mode
    let program = System.Console.ReadLine() |> Parser.parseProgram
    while true do
        try
            let input = System.Console.ReadLine() |> System.UInt64.Parse
            printfn "%d" (program |> Solver.eval input)
        with
        | e -> printfn "%s" (e.ToString())
    0

[<EntryPoint>]
let main argv = 
    //printfn "%A" argv
    match argv.[0] with
        | "--eval" -> eval ()
        | "--solve" -> 1 // sooon
        | _ -> 127

