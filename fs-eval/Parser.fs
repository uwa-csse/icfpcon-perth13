module Parser
open Type


type 'a RevList = 
    | Empty
    | Cons of 'a RevList * 'a
    static member ofList l : 'a RevList = List.fold (fun s elem -> Cons(s, elem)) Empty l
    static member ofSeq s : 'a RevList = Seq.fold (fun s elem -> Cons(s, elem)) Empty s
    static member fold fn s (rl:'a RevList) = 
        let rec helper fn s = function
            | Empty -> s
            | Cons(next, elem) -> helper fn (fn s elem) next
        helper fn s rl
    static member toList (rl:'a RevList) = rl.asList
    static member toSeq (rl:'a RevList) = rl.asSeq
    static member toArray (rl:'a RevList) = rl.asArray
    member this.asSeq = Seq.ofList this.asList
    member this.asList = RevList.fold (fun s e -> e::s) [] this
    member this.asArray = Array.ofList this.asList

type Token = 
    | Id of string
    | Zero
    | One
    | Expr of Token list

let (|Digit|_|) d = if d >= '0' && d <= '9' then Some(int d - int '0') else None
let (|Ident|_|) i = if (i >= 'a' && i <= 'z') || i = '_' || System.Char.IsDigit(i) then Some(i) else None
let (|WhiteSpace|_|) c = if System.Char.IsWhiteSpace c then Some(c) else None

let rec lexify stream = 
    let rec parseIdent stream i = 
        let rec helper acc = function
            | Ident(i) :: t -> helper (RevList.Cons(acc, i)) t
            | left -> (Id(new string(RevList.toArray acc)), left)
        helper (RevList.Cons(RevList.Empty, i)) stream
    let rec parseWs = function
        | WhiteSpace(_) :: t -> parseWs t
        | next -> next
    let parseExpr stream = 
        let rec helper acc counter = function
            | [] -> failwith "Bracket mismatch"
            | ')' :: t when counter = 0 -> (lexify (RevList.toList acc), t)
            | ')' :: t -> helper (RevList.Cons(acc, ')')) (counter-1) t
            | '(' :: t -> helper (RevList.Cons(acc, '(')) (counter+1) t
            | h :: t -> helper (RevList.Cons(acc, h)) counter t
        helper RevList.Empty 0 stream
    let rec helper toks = function
        | [] -> RevList.toList toks
        | '(' :: t -> 
            let (tok, next) = parseExpr t
            helper (RevList.Cons(toks, tok)) next
        | '0' :: t -> helper (RevList.Cons(toks, Zero)) t
        | '1' :: t -> helper (RevList.Cons(toks, One)) t
        | Ident(i) :: t -> 
            let (tok, next) = parseIdent t i
            helper (RevList.Cons(toks, tok)) next
        | WhiteSpace(_) :: t -> helper toks (parseWs t)
        | h :: t -> failwith (sprintf "Unrecognized symbol: %A" h)
    Expr(helper RevList.Empty stream)

let BinIds = Set.ofList ["or"; "xor"; "and"; "plus";]
let UnIds = Set.ofList ["or"; "xor"; "and"; "plus";] 

let (|UnOp|_|) unop = 
    match unop with
    | "not" -> Some Not
    | "shl1" -> Some Shl1
    | "shr1" -> Some Shr1
    | "shr4" -> Some Shr4
    | "shr16" -> Some Shr16
    | _ -> None
let (|BinOp|_|) binop = 
    match binop with
    | "or" -> Some Or
    | "xor" -> Some Xor
    | "and" -> Some And
    | "plus" -> Some Plus
    | _ -> None
let makeExpr (ex:Expr) : ExprChild = {id = -1; level = 255uy; e = ex}

let parseProgram stream = 
    let rec parseTok vars = function
        | Zero -> Atom(Type.Zero)
        | One -> Atom(Type.One)
        | Id(i) when Map.containsKey i vars -> Atom(Type.Id(Map.find i vars))
        | Expr(Id("if0") :: cond :: tExp :: fExpr :: []) ->
            If0(parseTok vars cond |> makeExpr, parseTok vars tExp |> makeExpr, parseTok vars fExpr |> makeExpr)
        | Expr(Id("fold") :: a :: b :: Expr(Id("lambda") :: Expr(Id(arg1) :: Id(arg2) :: []) :: body :: []) :: []) ->
            let parsedA = parseTok vars a
            let parsedB = parseTok vars b
            Fold((parsedA |> makeExpr, parsedB |> makeExpr), parseTok ( vars.Add (arg1, Type.Y) |> Map.add arg2 Type.Z ) body |> makeExpr)
        | Expr(Id(BinOp(op)) :: a :: b :: []) ->
            Binary(op, parseTok vars a |> makeExpr, parseTok vars b |> makeExpr)
        | Expr(Id(UnOp(op)) :: a :: []) ->
            Unary(op, parseTok vars a |> makeExpr)
        | Expr(a :: []) -> parseTok vars a
        | a -> printfn "%A" a; failwith "Invalid expr"
    match lexify (List.ofSeq stream) with
    | Expr(Expr(Id("lambda") :: Expr(Id(x) :: []) :: body :: []) :: []) -> parseTok (Map.ofList [(x, Type.X)]) body
    | a -> 
        printfn "%A" a
        failwith "Invalid starting lambda"