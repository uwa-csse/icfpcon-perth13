module Solver
open Type

let idIndex id = 
    match id with
    | X -> 0
    | Y -> 1
    | Z -> 2

///Determine the 64bit value of an atom
let atomValue (x,y,z) = function
        | Zero -> 0UL
        | One -> 1UL
        | Value(v) -> v
        | Id(id) ->
            match id with
            | X -> x
            | Y -> y
            | Z -> z

let rec evalExpr (x,y,z) body = 
        let env = (x,y,z)
        match body with
        | Atom(a) -> atomValue env a
        | If0(cond, ifExpr, elseExpr) -> 
            if (evalExpr env cond.e = 0UL) then evalExpr env ifExpr.e else evalExpr env elseExpr.e
        | Fold((init, acc), lambda) -> 
            let foldee = System.BitConverter.GetBytes(evalExpr env init.e)
            let accBytes = evalExpr env acc.e
            Array.fold (fun s t -> evalExpr (x, uint64 t, s) lambda.e) accBytes foldee
        | Unary(op, arg) -> 
            let bits = evalExpr env arg.e
            match op with
            | Not -> ~~~ bits
            | Shl1 -> bits <<< 1
            | Shr1 -> bits >>> 1
            | Shr4 -> bits >>> 4
            | Shr16 -> bits >>> 16
        | Binary(op, arg1, arg2) -> 
            let bits1, bits2 = evalExpr env arg1.e, evalExpr env arg2.e
            match op with
            | Plus -> bits1 + bits2
            | And -> bits1 &&& bits2
            | Or -> bits1 ||| bits2
            | Xor -> bits1 ^^^ bits2

///Evaluate a program for a given input
let eval input (program:Prog) = 
    evalExpr (input, uint64 0, uint64 0) program