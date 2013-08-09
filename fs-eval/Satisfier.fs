module Satisfier

type ValidOp = 
    | Not
    | Shl1
    (*| Shr1
    | Shr4
    | Shr16
    | And
    | Or
    | Xor
    | Plus*)

type ValidLiterals = 
    | Zero
    | One
    | X
    //| Y
    //| Z

let atoms = [Type.Atom(Type.Zero); Type.Atom(Type.One); Type.Atom(Type.Id(Type.X))]