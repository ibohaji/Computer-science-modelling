module Parse
open FSharp.Text.Lexing
open System
open AST

exception ParseError of Position * string * Exception

let parse parser src =
    let lexbuf = LexBuffer<char>.FromString src

    let parser = parser Lexer.tokenize

    try
        Ok(parser lexbuf)
    with
    | e ->
        let pos = lexbuf.EndPos
        let line = pos.Line
        let column = pos.Column
        let message = e.Message
        let lastToken = new String(lexbuf.Lexeme)
        eprintf "Parse failed at line %d, column %d:\n" line column
        eprintf "Last token: %s" lastToken
        eprintf "\n"
        Error(ParseError(pos, lastToken, e))

let rec prettyPrintCExp cexp = 
        match cexp with
        | Assign (x,y) -> "" + prettyPrintVar x + ":=" + prettyPrintAExp y
        | Skip -> "skip"
        | C (c1,c2) -> prettyPrintCExp c1 + ";" + prettyPrintCExp c2
        | If gc -> "if  " + prettyPrintGCExp gc + "  fi"
        | Do gc -> "do " + prettyPrintGCExp gc + "\nod"
    and prettyPrintVar var =
        match var with
        | Var v -> v
        | Array (x,y) -> x + "[" + prettyPrintAExp y + "]"
    and prettyPrintAExp aexp = 
        match aexp with
        | Num i -> string i
        | V var -> prettyPrintVar var
        | Plus (x,y) -> prettyPrintAExp x + "+" + prettyPrintAExp y 
        | Minus (x,y) -> prettyPrintAExp x + "-" + prettyPrintAExp y 
        | Mult (x,y) -> prettyPrintAExp x + "*" + prettyPrintAExp y 
        | Div (x,y) -> prettyPrintAExp x + "/" + prettyPrintAExp y 
        | UMinus x -> "-" + prettyPrintAExp x 
        | Pow (x,y) -> prettyPrintAExp x + "^" + prettyPrintAExp y
    and prettyPrintGCExp gcexp =
        match gcexp with
        | Then (b,c) -> prettyPrintBExp b + " -> " + prettyPrintCExp c
        | GC (gc1,gc2) -> prettyPrintGCExp gc1 + "\n[] " + prettyPrintGCExp gc2
    and prettyPrintBExp bexp = 
        match bexp with
        | True -> "true"
        | False -> "false"
        | SAnd (b1: bexp,b2) -> "(" + prettyPrintBExp b1 + "&&" + prettyPrintBExp b2 + ")"
        | SOr (b1,b2) -> "(" + prettyPrintBExp b1 + "||" + prettyPrintBExp b2 + ")"
        | And (b1,b2) -> "(" + prettyPrintBExp b1 + "&" + prettyPrintBExp b2 + ")"
        | Or (b1,b2) -> "(" + prettyPrintBExp b1 + "|" + prettyPrintBExp b2 + ")"
        | Not b -> "!" + "(" + prettyPrintBExp b + ")"
        | Eq (a1,a2) -> "(" + prettyPrintAExp a1 + "=" + prettyPrintAExp a2 + ")"
        | Neq (a1,a2) -> "(" + prettyPrintAExp a1 + "!=" + prettyPrintAExp a2 + ")"
        | Gt (a1,a2) -> "(" + prettyPrintAExp a1 + ">" + prettyPrintAExp a2 + ")"
        | Geq (a1,a2) ->  "(" + prettyPrintAExp a1 + ">=" + prettyPrintAExp a2 + ")"
        | Lt (a1,a2) -> "(" + prettyPrintAExp a1 + "<" + prettyPrintAExp a2 + ")"
        | Leq (a1,a2) -> "(" + prettyPrintAExp a1 + "<=" + prettyPrintAExp a2 + ")"






let rec prettyPrint ast =
   // TODO: start here
   prettyPrintCExp ast
    

let analysis (src: string) : string =
    match parse Parser.startGCL (src) with
        | Ok ast ->
            Console.Error.WriteLine("> {0}", ast)
            prettyPrint ast
        | Error e -> "Parse error: {0}"