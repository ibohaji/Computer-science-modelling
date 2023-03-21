module Graph
open Types
open Parse

open Parser
open FSharp.Text.Lexing
open System
open AST





type Determinism = Deterministic of string | NonDeterministic of string

type Input =
    { Case : string }



type Output = { dot: string }

type Node = string
type Label = K of cexp | B of bexp | S
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





let rec edgesDeterminstic(ast,qS,qF) = 
    match ast with 
        | Skip -> [{source = qS ; label = S; target = qF}] 
        | Assign(_,_) -> [{source = qS; label = S ; target = qF}]
        | C(c1,c2) -> 
                            let q = getNextNodeName()
                            edgesDeterminstic(c1,qS,q) @ edgesDeterminstic(c2,q,qF)
        | Do gc -> edgesGCD(gc,qS,qF) 
        | If gc -> edgesGCD(gc,qS,qF)

and edgesGCD(ast: gcexp,qS,qF) = 
        match ast with
                | Then(b,c) ->  
                            let q = getNextNodeName()
                            [{source =qS ; label = B(And(b,Not(b))) ; target=q}] @ edgesDeterminstic(c,qS,qF)
                | GC (gc1,gc2) -> edgesGCD(gc1,qS,qF) @ edgesGCD(gc2,qS,qF)



let rec edgesC(ast: cexp, qS:Node,qF:Node): List<Edge>  = 
    match ast with
        | Skip ->  [{source =qS ; label = S ; target=qF}]
        | Assign(_,_) -> [{source = qS; label = K ast; target =qF}]
        | C (c1,c2) -> 
             let q = getNextNodeName()
             edgesC(c1, qS, q) @ edgesC(c2,q,qF)
        | If gc ->   edgesGC(gc,qS,qF) 
        | Do gc ->  edgesGC(gc,qS,qF) //@ doneGC(gc,qS,qF)

        

and edgesGC (ast, qS, qF) = 
    match ast with 
        | Then(b,c) -> 
                let q1 = getNextNodeName() 
                [{source =qS ; label = B b ; target=q1}]  @ edgesC(c,q1,qS) @  [{source =qS ; label = B (Not b); target=qF}]
        | GC(gc1,gc2) -> let q1 = getNextNodeName()
                         let q2 = getNextNodeName()
                         edgesGC(gc1,qS,q1) @ edgesGC(gc2,q2,q1)






let ast2pg(ast,input): List<Edge>  = 
        match input with
            |{Case = "Determinstic"} -> edgesDeterminstic(ast,"q0","qf")
            |{Case = "NonDeterministic"} ->  edgesC(ast,"q0","qf")
            |_ -> failwith "Not a valid case"

   



let label2dot(l:Label) = 
    match l with 
        | B exp -> prettyPrintBExp(exp) 
        | K exp -> prettyPrintCExp(exp)
        | _ -> prettyPrint(Skip)


let edge2dot(e: Edge) : string =
    e.source + " -> " + e.target + "[label=" + label2dot(e.label)  + "]" + ";"

let rec edges2dot (pg : List<Edge>): string = 
    match pg with
        | [] -> ""
        | e::pg -> edge2dot(e) + edges2dot(pg)



let pg2dot (pg : List<Edge>): string = 
   "digraph program_graph { rankdir=LR;" + edges2dot(pg)+ " }"



let analysis (src: string) (input: Input) : Output =
    match parse Parser.startGCL (src) with
        | Ok ast ->
            let pg = ast2pg(ast,input)
          //  Console.Error.WriteLine("> {0}",pg)
            let dotstring = pg2dot(pg)
            { dot = dotstring }

        | Error e -> { dot  = ""}
