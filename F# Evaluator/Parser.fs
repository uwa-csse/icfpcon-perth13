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
let (|Ident|_|) i = if i >= 'a' && i <= 'z' then Some(i) else None
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
        | '1' :: t -> helper (RevList.Cons(toks, Zero)) t
        | Ident(i) :: t -> 
            let (tok, next) = parseIdent t i
            helper (RevList.Cons(toks, tok)) next
        | WhiteSpace(_) :: t -> helper toks (parseWs t)
        | h :: t -> failwith (sprintf "Unrecognized symbol: %A" h)
    Expr(helper RevList.Empty stream)


let parse stream = 
    let rec parseExpr = function
        | Id("If0") :: Expr(cond) :: Expr(tExp) :: Expr(fExpr) :: [] ->
            If0(parseExpr cond, parseExpr tExp, parseExpr fExpr)
        | _ -> failwith "Invalid expr"
    match lexify (List.ofSeq stream) with
    | Expr(Expr(Id("lambda") :: Expr(Id("x") :: Expr(body) :: []) :: t) :: []) -> parseExpr body
    | _ -> failwith "Invalid starting lambda"