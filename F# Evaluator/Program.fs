
[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    while true do
        try
            printfn "%A" (Parser.parseProgram (System.Console.ReadLine()))
        with
        | e -> printfn "%s" (e.ToString())
    0 // return an integer exit code
