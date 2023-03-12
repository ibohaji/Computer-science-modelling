module Graph
open Types
open Parse

open Parser
open FSharp.Text.Lexing
open System
open AST





type Input = { 
    determinism: Determinism
    }

type Output = { dot: string }


type Node = string
type Label =  cexp
type Edge = {
    source : Node;
    label : Label;
    target : Node;
}

let mutable nodeCount = 1
let getNextNodeName() = 
    let nodeName = "q" + string nodeCount
    nodeCount <- nodeCount+1
    nodeName

let rec edges(ast: cexp, qS:Node,qF:Node): List<Edge>  = 
    let q = getNextNodeName()  
    match ast with
        | Skip -> [{source =qS ; label =ast ; target=qF}]
        | Assign(_,_) -> [{source = qS; label = ast; target =qF}]
        | C(c1,c2) ->  edges(c1,qS,q) @ edges(c2,q,qF)
        | If gc ->
            match gc with
            | GC(c1,c2) ->  [{source = qS; label = ast; target =qF}]
            | Then(c1,c2) -> [{source = qS; label = ast; target =qF}]


        | Do(c1) -> [{source =qS ; label =ast ; target=qF}]
        
        //stil need to implement more stuff here
      //  | Do gc -> [{ source = qS; label = ast; target = q }]@ edges(gc,q,qF)




let ast2pg(ast: cexp): List<Edge>  = 
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
