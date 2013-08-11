module Type

//Type system

///Unary operators
type UnOp = 
    | Not
    | Shl1
    | Shr1
    | Shr4
    | Shr16

///Represents a variable identifier
type Identifier = 
    | X
    | Y
    | Z

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
    | Value of uint64
    | Id of Identifier

///Program Expressions. Note: Lambdas are special cases
type Expr = 
    | Atom of Atomic
    | If0 of ExprChild * ExprChild * ExprChild
    | Fold of (ExprChild * ExprChild) * ExprChild
    | Unary of UnOp * ExprChild
    | Binary of BinOp * ExprChild * ExprChild
and ExprChild = { id : int; level : byte; e : Expr }



type Prog = Expr