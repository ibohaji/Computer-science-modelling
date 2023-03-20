module Graph
open Types
open Parse

open Parser
open FSharp.Text.Lexing
open System
open AST



let edges(ast,qS,qF) =
    match ast with 
        |skip -> [{qS ; ast; qF}]
        | Assign(c,x) -> [{qS; ast; qF}]
        | C(c1,c2) ->  [{qS; c1; qF}]@[{qS; c2 ; qF}]
        | If(Then(b,c)) ->  [{qS; b; qF}]@ [{qS; c; qF}] @ [{qS; "not"+b; qF}]
    
let ast2pg(ast) = 
    edges(ast,"q0","qf")




let analysis (src: string) (input: Input): OutPut = 
    match parse Parser.startGCL (src) with 
        | Ok ast ->
            let pg = ast2pg(ast)

            let dotstring = pg2dot(pg)
        | Error e -> {dot =""}