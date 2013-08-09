module Satisfier

type ValidOp = 
    | Not
    | Shl1
    | Shr1
    | Shr4
    | Shr16
    //| And
    //| Or
    //| Xor
    //| Plus

type ValidLiterals = 
    | Zero
    | One
    | X
    //| Y
    //| Z

let atoms = [Type.Atom(Type.Zero); Type.Atom(Type.One); Type.Atom(Type.Id(Type.X))]

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