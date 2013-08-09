module Deparser
open Type

///Determine the string representation of a unary operator
let unOpName (u:UnOp) = 
    match u with
        | Not -> "not"
        | Shl1 -> "shl1"
        | Shr1 -> "shr1"
        | Shr4 -> "shr4"
        | Shr16 -> "shr16"
///Determine the string representation of a binary operator
let binOpName (b:BinOp) = 
    match b with
        | And -> "and"
        | Or -> "or"
        | Xor -> "xor"
        | Plus -> "plus"

let idName id = 
    match id with
    | X -> "x"
    | Y -> "y"
    | Z -> "z"

///Determine the string representation of an expression
let exprAsStr expr = 
    let rec helper acc = function
        | If0(a,b,c) -> acc + "(if0 " + (helper "" a) + (helper "" b) + (helper "" c) + ")"
        | Fold((a,b),c) -> acc + "(fold " + (helper "" a) + (helper "" b) + (helper "" c) + ")"
        | Unary(op, expr) -> acc + "(" + unOpName op + " " + (helper "" expr) + ")"
        | Binary(op, expr, expr2) -> acc + "(" + binOpName op + " " + (helper "" expr) + " " + (helper "" expr2) + ")"
        //| Lambda1(arg, body) -> acc + "(" + "lambda (" + idName arg + ") " + (helper "" body) + ")"
        //| Lambda2((arg1, arg2), body) -> acc + "(" + "lambda (" + idName arg1 + " " + idName arg2 + ") " + (helper "" body) + ")"
        | Atom(a) ->
            match a with
            | Zero -> "0"
            | One -> "1"
            | Id(id) -> 
                match id with
                | X -> "x"
                | Y -> "y"
                | Z -> "z"
            | _ -> failwith "Invalid atom"
    helper "" expr

let progAsStr (prog:Prog) = 
    "( lambda (x) " + exprAsStr prog + ")"