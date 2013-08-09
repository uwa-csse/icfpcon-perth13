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
    | If0 of Expr * Expr * Expr
    | Fold of (Expr * Expr) * Expr
    | Unary of UnOp * Expr
    | Binary of BinOp * Expr * Expr


type Prog = Expr