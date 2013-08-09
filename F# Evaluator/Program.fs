
//Type system

///Unary operators
type UnOp = 
    | Not
    | Shl1
    | Shr1
    | Shr4
    | Shr16

///Binary operators
type BinOp = 
    | And
    | Or
    | Xor
    | Plus

///Atoms/Values
type Atomic =
    | Zero
    | One
    | Value of int64
    | Id of string

///Program Expressions. Note: Lambdas are special cases
type Expr = 
    | Atom of Atomic
    | If0 of Expr * Expr * Expr
    | Fold of (Expr * Expr) * Expr
    | Unary of UnOp * Expr
    | Binary of BinOp * Expr * Expr
    | Lambda1 of string * Expr
    | Lambda2 of (string * string) * Expr

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

///Determine the string representation of an expression
let exprAsStr expr = 
    let rec helper acc = function
        | If0(a,b,c) -> acc + "(if0 " + (helper "" a) + (helper "" b) + (helper "" c) + ")"
        | Fold((a,b),c) -> acc + "(fold " + (helper "" a) + (helper "" b) + (helper "" c) + ")"
        | Unary(op, expr) -> acc + "(" + unOpName op + " " + (helper "" expr) + ")"
        | Binary(op, expr, expr2) -> acc + "(" + binOpName op + " " + (helper "" expr) + " " + (helper "" expr2) + ")"
        | Lambda1(arg, body) -> acc + "(" + "lambda (" + arg + ") " + (helper "" body) + ")"
        | Lambda2((arg1, arg2), body) -> acc + "(" + "lambda (" + arg1 + " " + arg2 + ") " + (helper "" body) + ")"
        | Atom(a) ->
            match a with
            | Zero -> "0"
            | One -> "1"
            | Id(str) -> str
            | _ -> failwith "Invalid atom"
    helper "" expr

///Determine the 64bit value of an atom
let atomValue (env : Map<string, int64>) = function
        | Zero -> int64 0
        | One -> int64 1
        | Value(v) -> v
        | Id(str) -> 
            match env.TryFind str with
            | Some(v) -> v
            | None -> failwith (sprintf "Invalid lookup for id %s" str) 

///Evaluate a program for a given input
let eval input program = 
    let rec evalExpr env = function
        | Atom(a) -> atomValue env a
        | If0(cond, ifExpr, elseExpr) -> 
            if (evalExpr env cond = int64 0) then evalExpr env ifExpr else evalExpr env elseExpr
        | Fold((init, acc), lambda) -> int64 0
        | Unary(op, arg) -> 
            let bits = evalExpr env arg
            match op with
            | Not -> ~~~ bits
            | Shl1 -> bits <<< 1
            | Shr1 -> bits >>> 1
            | Shr4 -> bits >>> 4
            | Shr16 -> bits >>> 16
        | Binary(op, arg1, arg2) -> 
            let bits1, bits2 = evalExpr env arg1, evalExpr env arg2
            match op with
            | Plus -> bits1 + bits2
            | And -> bits1 &&& bits2
            | Or -> bits1 ||| bits2
            | Xor -> bits1 ^^^ bits2
        | Lambda1(_) -> failwith "Can't eval a lambda directly"
        | Lambda2(_) -> failwith "Can't eval a lambda directly"

    match program with
    | Lambda1(arg, code) -> // we expect only a lambda with one arguement, otherwise it is a malformed program
        evalExpr (Map.ofList [(arg, input)]) code
    | _ -> failwith "Invalid program"


[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
