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
    //| Fold
    //| TFold
    | If0

type ValidLiterals = 
    | Zero
    | One
    | X
    //| Y
    //| Z

let atoms = [Type.Atom(Type.Zero); Type.Atom(Type.One); Type.Atom(Type.Id(Type.X))]

//opsUsed will be a bitset represing which operators have already been used
type Satisfied = {opsUsed : int; program : Type.Expr; folded : bool }

let zeroProg = { opsUsed = 0; program = Type.Atom(Type.Zero); folded = false }
let oneProg = { zeroProg with program = Type.Atom(Type.One) }
let xProg = { zeroProg with program = Type.Atom(Type.Id(Type.X)) }
let yProg = { zeroProg with program = Type.Atom(Type.Id(Type.Y)); folded = true }
let zProg = { yProg with program = Type.Atom(Type.Id(Type.X)) }
let singletonPrograms = [| zeroProg; oneProg; xProg |]
let singletonProgramsFolded = [| zeroProg; oneProg; xProg; zProg  |]

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


// A bottom up approach to generating all programs up to a given size
// ToDo: opsUsed needs to be implemented
let bottomUp unOpSet binOpSet targetSize = 
    let sols = Array.create targetSize [||]
    sols.[0] <- singletonPrograms
    for size in 2..targetSize do
        sols.[size-1] <- [|
            //generate unary operators which only 1 to the size
            for sol in sols.[size-2] do
                match sol with
                | { program = p } as sat -> 
                    for op in unOpSet -> 
                        {sat with program = Type.Unary(unOpCode op, p)}
            //generate binary operators
            for i in 1..(size-2) do //loop through all left-hand expression sizes for a binary operator
                let j = (size-1) - i //calculate the right hand expr size
                for op in binOpSet do
                    for iExpr in sols.[i-1] do
                        for jExpr in sols.[j-1] ->
                            {iExpr with program = Type.Binary(binOpCode op, iExpr.program, jExpr.program); folded = iExpr.folded || jExpr.folded }

            //generate if0 expr
            for i in 1..(size-3) do
                for j in 1..(size-3) do
                    for k in 1..(size-3) do
                        if i + j + k = targetSize - 1 then
                            for iExpr in sols.[i-1] do
                                for jExpr in sols.[j-1] do
                                    for kExpr in sols.[k-1] ->
                                        {iExpr with program = Type.If0(iExpr.program, jExpr.program, kExpr.program); folded = iExpr.folded || jExpr.folded || kExpr.folded }
            //generate fold expr
            for i in 1..(size-4) do
                for j in 1..(size-4) do
                    for k in 1..(size-4) do
                        if i + j + k = targetSize - 1 then
                            for iExpr in sols.[i-1] |> Array.filter (fun p -> not p.folded) do
                                for jExpr in sols.[j-1] |> Array.filter (fun p -> not p.folded) do
                                    for kExpr in sols.[k-1] |> Array.filter (fun p -> p.folded) ->
                                        if size = targetSize then () //this is a top level fold
                                        {iExpr with program = Type.Fold((iExpr.program, jExpr.program), kExpr.program); folded = true}
                    
        |]
    Array.map (fun {program = p} -> p) sols.[targetSize-1]

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