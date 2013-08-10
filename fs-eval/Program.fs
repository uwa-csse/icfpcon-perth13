
let eval () =
    let program = System.Console.ReadLine() |> Parser.parseProgram
    let mutable d = true
    while d do
        try
            let input = System.Console.ReadLine()
            if input = null then d <- false else
                printfn "%d" (program |> Solver.eval (System.UInt64.Parse (input)))
        with
        | e -> printfn "%s" (e.ToString())
    0

[<EntryPoint>]
let main argv = 
    printfn "%A" (Satisfier.bottomUp [Satisfier.Shl1; Satisfier.Shr1] [Satisfier.And] 6)
    System.Console.ReadLine()
    //printfn "%A" argv
    match argv.[0] with
        | "--eval" -> eval ()
        | "--solve" -> 1 // sooon
        | _ -> 127

