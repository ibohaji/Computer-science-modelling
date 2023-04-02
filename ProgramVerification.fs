module ProgramVerification

open System
open Predicate.AST

(*
    This defines the input and output for the program verification analysis.
    Please do not change the definitions below as they are needed for the
    validation and evaluation tools!
*)

type Input = unit

type Output =
    { verification_conditions: List<SerializedPredicate> }

let analysis (src: string) (input: Input) : Output =
    let (P, C, Q) =
        match Predicate.Parse.parse src with
        | Ok (AnnotatedCommand (P, C, Q)) -> P, C, Q
        | Error e ->
            failwith
                $"Failed to parse.\n\nDid you remember to surround your program with predicate blocks, like so?\n\n  {{ true }} skip {{ true }}\n\n{e}"



    let rec subst(p,v: string,e:AExpr): Predicate = 
        match p with 
        // just a test more logic is needed for the first clause to substitute the needed expressions with a fresh variable
        | RelationalOp(a1,r,a2) -> BooleanOp(RelationalOp(a1,r,a2),And,RelationalOp(Variable(v),Eq,e))
        |_ -> failwith"notyet"

    let rec sPC(C:Command,P:Predicate): Predicate =
        match C with 
        | Skip -> P
        | Assign(string,expr) -> subst(P,string,expr)
        | Sep(c1,c2) -> sPC(c1,sPC(c2,P))
        | If gc -> failwith"notyet"
        | Do(p,gc) -> failwith "notyet"
        |_ -> failwith "not implmnted yet"

    let verification_conditions: List<Predicate> = [BooleanOp(sPC(C,P),Implies,Q)]@[]
    // Let this line stay as it is.
    { verification_conditions = List.map serialize_predicate verification_conditions }
