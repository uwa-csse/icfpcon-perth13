module Satisfier

open System.Collections.Generic

type ConHashSet() = 
    let hs = HashSet<uint64>()
    member this.Add(item) = 
        lock hs (fun () ->
            hs.Add(item)
        )

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

type Satisfied = Type.Expr

let zeroProg = Type.Atom(Type.Zero) 
let oneProg = Type.Atom(Type.One)
let xProg = Type.Atom(Type.Id(Type.X))
let yProg = Type.Atom(Type.Id(Type.Y))
let zProg = Type.Atom(Type.Id(Type.Z))
let singletonPrograms = [| zeroProg; oneProg; xProg; yProg; zProg |]

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


let rnd = System.Random()
let ios = Array.init 16 (fun _ -> (rnd.Next() |> uint64, ConHashSet() ) )


let groupsOfAtMost (size: int) (s: seq<'v>) : seq<array<'v>> =
  seq { let en = s.GetEnumerator ()
        let more = ref true
        while !more do
            let i = ref 0
            let group = [|  let i = ref 0
                            while !i < size && en.MoveNext () do
                            yield en.Current
                            i := !i + 1 |]
            if group.Length=0 then
                more := false
            else
                yield group
    }

//this function does a quick cull of satisfied programs
//it just tries some random inputs, and gets rid of programs that have no unique outputs
//Its extremely parallel
let quickCull sats = 
    //printfn "CULL!"
    Array.Parallel.map (
        fun prog -> 
            let inserts = Array.map (fun (input, (oSet:ConHashSet) ) -> 
                //printfn "%d -> %d   %A" input (Solver.eval input prog) prog
                oSet.Add(Solver.eval input prog)) ios
            if Array.tryFind id inserts |> Option.isSome then Some(prog)
            else None
    ) sats
    |> Array.filter (fun a -> Option.isSome a) |> Array.map (fun a -> Option.get a)

// A bottom up approach to generating all programs up to a given size. Uses an approach resembling a bottom up DP.
// Note: does not deal with TFold, as it cannot know if a program you are considering is at the "top" level
// Note: this is by far the most ridiculously imperitive looking code I've ever written in F#
let bottomUp maxSize = 
    let sols = Array.create maxSize Array.empty
    let unOpSet = [Not;Shl1;Shr1;Shr4;Shr16]
    let binOpSet = [And;Or;Xor;Plus]
    sols.[0] <- singletonPrograms |> quickCull
    for size in 2..maxSize do
        printfn "%d" size
        sols.[size-1] <-
         seq {
            //generate unary operators which only 1 to the size
            for sol in sols.[size-2] do
                for op in unOpSet -> Type.Unary(unOpCode op, sol)
            //generate binary operators
            for i in 1..(size-2) do //loop through all left-hand expression sizes for a binary operator
                let j = (size-1) - i //calculate the right hand expr size
                for op in binOpSet do
                    for iExpr in sols.[i-1] do
                        for jExpr in sols.[j-1] do if iExpr <= jExpr then yield Type.Binary(binOpCode op, iExpr, jExpr)

            //generate if0 expr
            for i in 1..(size-3) do
                for j in 1..(size-3) do
                    for k in 1..(size-3) do
                        if i + j + k = maxSize - 1 then
                            for iExpr in sols.[i-1] do
                                for jExpr in sols.[j-1] do
                                    for kExpr in sols.[k-1] -> Type.If0(iExpr, jExpr, kExpr)
         } |> groupsOfAtMost 1024   |> Seq.map quickCull |> Seq.concat |> Array.ofSeq

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