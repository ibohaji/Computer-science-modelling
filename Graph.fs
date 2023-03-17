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
type Label = B of bexp | C of cexp 

type Edge = {
    source : Node;
    label : Label;
    target : Node;
    }


let mutable currentNodeId = 1

let new_node()=
    let nodeId = "q" + string currentNodeId
    currentNodeId <- currentNodeId + 1
    nodeId


let rec edges(ast: Label, qS:Node,qF:Node): List<Edge>  = 
    match ast with
        | Skip -> [{source =qS ; label =ast ; target=qF}]
        | Assign(_,_) -> [{source = qS; label = ast; target =qF}]
        | C(c1,c2) ->              let q = new_node()
                                   List.concat [edges(c1, qS, q); edges(c2, q , qF)]
        | If gc -> 
            match gc with
            |Then(b,c) -> [{source = qS; label = b; target =qF}]@[{source = qS; label = c; target =qF}]
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
