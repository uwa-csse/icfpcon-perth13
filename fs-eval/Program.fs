
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
    let sz = 12
    let progs = Satisfier.bottomUp sz
    printfn "%d" ( progs  |> Array.sumBy (fun s -> Seq.length s) )
    let tnow = System.DateTime.Now
    let tstr = string tnow.Hour + string tnow.Minute + "." + string tnow.Second
    for i=1 to sz do
        System.IO.File.WriteAllLines("eqclasses" + string i + "-" + tstr + ".txt",
                                     progs.[i-1] |> Seq.map Deparser.exprAsStr )
    //Satisfier.bottomUp 3 |> Array.iter (Seq.iter (printfn "%A"))
    printfn "done"
    System.Console.ReadLine()
    //printfn "%A" argv
    match argv.[0] with
        | "--eval" -> eval ()
        | "--solve" -> 1 // sooon
        | _ -> 127

