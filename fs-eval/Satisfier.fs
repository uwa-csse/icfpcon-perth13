module Satisfier

type ValidUnOp = 
    | Not
    | Shl1
    | Shr1
    | Shr4
    | Shr16

type ValidBinOp = 
    | And
    | Or
    | Xor
    | Plus

type ValidSpecialOp = 
    | Fold
    //| TFold
    | If0


let atoms = [Type.Atom(Type.Zero); Type.Atom(Type.One); Type.Atom(Type.Id(Type.X))]

type Satisfied = {
    opsUsed : int;  //bitset representing operators used
    program : Type.Expr; ///the program
    folded : bool  //whether or not the program has been folded
}

let zeroProg = { opsUsed = 0; program = Type.Atom(Type.Zero); folded = false }
let oneProg = { zeroProg with program = Type.Atom(Type.One) }
let xProg = { zeroProg with program = Type.Atom(Type.Id(Type.X)) }
let yProg = { zeroProg with program = Type.Atom(Type.Id(Type.Y)); folded = true }
let zProg = { yProg with program = Type.Atom(Type.Id(Type.X)) }
let singletonPrograms = [| zeroProg; oneProg; xProg; zProg  |]

let unOpCode = function
    | Not -> Type.Not   
    | Shl1 -> Type.Shl1
    | Shr1 -> Type.Shr1
    | Shr4 -> Type.Shr4
    | Shr16 -> Type.Shr16

let binOpCode = function
    | And -> Type.And
    | Or -> Type.Or
    | Xor -> Type.Xor
    | Plus -> Type.Plus

//all these index functions are used to shift bits in the bitset
let unOpIndex = function
    | Not -> 0
    | Shl1 -> 1
    | Shr1 -> 2
    | Shr4 -> 3
    | Shr16 -> 4

let binOpIndex = function
    | And -> 5
    | Or -> 6
    | Xor -> 7
    | Plus -> 8

let specialOpIndex = function
    | Fold -> 9
    //| TFold
    | If0 -> 10


// A bottom up approach to generating all programs up to a given size. Uses an approach resembling a bottom up DP.
// Note: does not deal with TFold, as it cannot know if a program you are considering is at the "top" level
// Note: this is by far the most ridiculously imperitive looking code I've ever written in F#
let bottomUp maxSize = 
    let sols = Array.create maxSize [||]
    let unOpSet = [Not;Shl1;Shr1;Shr4;Shr16]
    let binOpSet = [And;Or;Xor;Plus]
    sols.[0] <- singletonPrograms
    for size in 2..maxSize do
        printfn "%d" size
        sols.[size-1] <- [|
            //generate unary operators which only 1 to the size
            for sol in sols.[size-2] do
                match sol with
                | { program = p } as sat -> 
                    for op in unOpSet -> 
                        {sat with 
                            program = Type.Unary(unOpCode op, p);
                            opsUsed = sat.opsUsed ||| (1 <<< unOpIndex op);}
            //generate binary operators
            for i in 1..(size-2) do //loop through all left-hand expression sizes for a binary operator
                let j = (size-1) - i //calculate the right hand expr size
                for op in binOpSet do
                    for iExpr in sols.[i-1] do
                        for jExpr in sols.[j-1] ->
                            {iExpr with 
                                program = Type.Binary(binOpCode op, iExpr.program, jExpr.program); 
                                opsUsed = (iExpr.opsUsed ||| jExpr.opsUsed) ||| (1 <<< binOpIndex op);
                                folded = iExpr.folded || jExpr.folded }

            //generate if0 expr
            for i in 1..(size-3) do
                for j in 1..(size-3) do
                    for k in 1..(size-3) do
                        if i + j + k = maxSize - 1 then
                            for iExpr in sols.[i-1] do
                                for jExpr in sols.[j-1] do
                                    for kExpr in sols.[k-1] ->
                                        {iExpr with 
                                            program = Type.If0(iExpr.program, jExpr.program, kExpr.program); 
                                            opsUsed = (iExpr.opsUsed ||| jExpr.opsUsed ||| kExpr.opsUsed) ||| (1 <<< specialOpIndex If0);
                                            folded = iExpr.folded || jExpr.folded || kExpr.folded }
            //generate fold expr
            for i in 1..(size-4) do
                for j in 1..(size-4) do
                    for k in 1..(size-4) do
                        if i + j + k = maxSize - 1 then
                            for iExpr in sols.[i-1] |> Array.filter (fun p -> not p.folded) do //Y cant be folded
                                for jExpr in sols.[j-1] |> Array.filter (fun p -> not p.folded) do //Z cant be folded
                                    for kExpr in sols.[k-1] |> Array.filter (fun p -> p.folded) -> //the inner program must be folded, since this is a fold
                                        {iExpr with 
                                            program = Type.Fold((iExpr.program, jExpr.program), kExpr.program); 
                                            opsUsed = (iExpr.opsUsed ||| jExpr.opsUsed ||| kExpr.opsUsed) ||| (1 <<< specialOpIndex Fold);
                                            folded = true} //set folded to true to prevent multiple folds
                    
        |]
    sols

let rec satisfy occurs opSet currSize targetSize = 
    let opOccurs = Array.create (List.length opSet) false
    [
        for op in opSet do
            let opCode = 
                match op with
                | Not -> Type.Not
                | Shl1 -> Type.Shl1
                | Shr1 -> Type.Shr1
                | Shr4 -> Type.Shr4
                | Shr16 -> Type.Shr16
            let newOcc = if List.tryFind ((=) op) occurs |> Option.isNone then op::occurs else occurs
            if currSize + 2 = targetSize then
                if (List.length newOcc) = (List.length opSet) then
                    for atom in atoms do
                        yield Type.Unary(opCode, atom)
            if currSize < targetSize then
                for sat in satisfy newOcc opSet (currSize + 2) targetSize do
                    yield Type.Unary(opCode, sat)
    ]

let test opSet inputs outputs size = 
    let ios = List.zip inputs outputs
    satisfy [] opSet 0 size |> List.filter (fun expr -> List.tryFind (fun (i,o) -> Solver.eval i expr <> o) ios |> Option.isNone)