module Graph
open Types
open Parse

open Parser

open FSharp.Text.Lexing
open System
open AST





type Input = { determinism: Determinism }
type Output = { dot: string }


type Node = string
type Label = cexp 
type Edge = {
    source : Node;
    label : Label;
    target : Node;
}



let edges(ast: cexp, qS:Node,qF:Node): List<Edge>  = 
    match ast with
        | Skip -> [{source =qS ; label =ast ; target=qF}]
        | Assign(_,_) -> [{source = qS; label = ast; target =qF}]
        | C(c) ->  [{source =qS ; label =ast ; target=qF}]
        | If gc -> [{source =qS ; label =ast ; target=qF}]
        | Do gc ->  [{source =qS ; label =ast ; target=qF}]
        
let ast2pg(ast): List<Edge>  = 
    edges(ast,"q0","qf")



let label2dot(l:Label) : string = 
    "[label=" + prettyPrint(l) + "]"


let edge2dot(e: Edge) : string =
    e.source + " -> " + e.target + label2dot(e.label) + " ;"


let rec edges2dot (pg : List<Edge>): string = 
    match pg with
        | [] -> ""
        | e::pg -> edge2dot(e) + edges2dot(pg)

let pg2dot (pg : List<Edge>): string = 
   "digraph program_graph { rankdir=LR;" + edges2dot(pg)+ " }"
let analysis (src: string) (input: Input) : Output =
    match parse Parser.startGCL (src) with
        | Ok ast ->
            let pg = ast2pg(ast)
          //  Console.Error.WriteLine("> {0}",pg)
            let dotstring = pg2dot(pg)
            { dot = dotstring }

        | Error e -> { dot  = ""}
