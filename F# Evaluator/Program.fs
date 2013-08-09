
[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    while true do
        try
            let input = System.Console.ReadLine() |> System.UInt64.Parse
            printfn "%d" (Parser.parseProgram (System.Console.ReadLine()) |> Solver.eval input |> int ) 
        with
        | e -> printfn "%s" (e.ToString())
    0 // return an integer exit code
