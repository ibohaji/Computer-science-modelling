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


    let mutable freshVarCounter = -1
    let freshVar() =
        freshVarCounter <- freshVarCounter + 1
        sprintf "_f%d" freshVarCounter
   



    let rec replaceVarWithFresh (e: AExpr) (v: string) (var: string): AExpr =
        match e with
        | Number(n) -> Number(n)
        | Variable(s) -> if s = v then Variable(var) else Variable(s)
        | LogicalVariable(s) -> if s = v then LogicalVariable(var) else LogicalVariable(s)
        | Array(s, i) -> Array(s, replaceVarWithFresh i v var)
        | LogicalArray(s, i) -> LogicalArray(s, replaceVarWithFresh i v var)
        | Binary(left, op, right) ->
            let newLeft = replaceVarWithFresh left v var
            let newRight = replaceVarWithFresh right v var
            Binary(newLeft, op, newRight)
        | Function(f) -> Function(f)
        



                    



    and fresh(a1: AExpr,r,a2,e: AExpr,v,freshVarName) = 
            let q = replaceVarWithFresh a1 v freshVarName
            let expr = replaceVarWithFresh e v freshVarName
            let newPredicate =  BooleanOp(RelationalOp(q, r, a2), LAnd, RelationalOp(Variable(v), Eq, expr))
            newPredicate
        

    and subst(p,v: string,e:AExpr,freshVarName): Predicate = 
        match p with 
        // just a test more logic is needed for the first clause to substitute the needed expressions with a fresh variable
        | RelationalOp(a1,r,a2) -> fresh(a1,r,a2,e,v,freshVarName)
        | BooleanOp(p1,b,p2: Predicate) ->
            let newP1: Predicate = subst(p1,v,e,freshVarName)
            let newP2: Predicate = subst(p2,v,e,freshVarName)
            BooleanOp(newP1,b,newP2)
        | Exists(s,P:Predicate) -> subst(P,v,e,freshVarName)
        |_ -> failwith "not yet bro"


    let varName = freshVar()

    let rec sPC(C:Command,P:Predicate): Predicate =
        match C with 
        | Skip -> P
        | Assign(string,expr) -> 
                Exists(varName,subst(P,string,expr,varName))
        | Sep(c1,c2) ->           
              Exists(varName,sPC(c2,sPC(c1,P)))
        | If gc -> failwith"notyet"
        | Do(p,gc) -> failwith "notyet"
        |_ -> failwith "not implmnted yet"

    let verification_conditions: List<Predicate> = [BooleanOp(sPC(C,P),Implies,Q)]@[]
    // Let this line stay as it is.
    { verification_conditions = List.map serialize_predicate verification_conditions }
