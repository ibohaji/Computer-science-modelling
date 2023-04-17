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
   
    let varPrime (n:int) = 
        let mutable k = 0
        let k = k+n
        "_f" + string(k)

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
        

    and newPredicate (P:Predicate) (f:String) (leftSide: string) (rightSide) :Predicate = 
        match P with 
            |RelationalOp(right,rop,left) -> 
                let newAexpr = replaceVarWithFresh right leftSide f
             //   let newRightSide = replaceVarWithFresh rightSide leftSide f
                let newP1 = RelationalOp(newAexpr,rop,left)
               // let newP2: Predicate = RelationalOp(Variable(leftSide),Eq,newRightSide)
                newP1
            |_ ->failwith"wth"



     and newArithmeticExp s e f = 
        let newA = replaceVarWithFresh e s f 
        newA 
   
        
    and VC (string:string) (expr:AExpr) (P:Predicate) (var:String):Predicate= 
        match P with 
            | Bool(b) -> P
            | RelationalOp(aexp1,rop,aexpr2) -> 
                let newp1 = newPredicate P var string expr
                newp1
            | BooleanOp(p1,bop,p2) -> 
                BooleanOp(VC string expr p1 var,bop,VC string expr p2 var)
            | Exists(v,p) ->Exists(v,VC string expr p var)
            | Forall(var,P) ->P
            | Not(P) -> P

    and DoneGC gc P var n = 
        match gc with 
         | Guard(bexpr,c) -> sPC(c,P,var,n)
         | Choice(gc1,gc2) -> failwith "notyet"


    and gcHandler gc P var n =
        match gc with 
            | Guard(b,c) -> sPC(c,BooleanOp(b,LAnd,P),var,n)
            | Choice(gc1,gc2) -> BooleanOp(gcHandler gc1 P var n,LOr,gcHandler gc2 P (varPrime(n+1)) (n+1))
    
    and sPC(C:Command,P:Predicate,var:String,n:int): Predicate =
        match C with 
        | Skip -> P
        | Assign(string,expr) ->
        //         let newA = newArithmeticExp string expr var
         let newA = RelationalOp(Variable(string),Eq,newArithmeticExp string expr var)
         Exists(var,BooleanOp(VC string expr P var,LAnd,newA))
        | Sep(c1,c2) ->
              sPC(c2,sPC(c1,P,varPrime(n+1),0),var,n+1)
        | If (gc:GuardedCommand) -> gcHandler gc P var n
        | Do(p,gc: GuardedCommand) -> DoneGC gc p var n
        | ArrayAssign(array,idx,aexpr) -> Exists(var,BooleanOp(P,LAnd,RelationalOp(LogicalArray(array,idx),Eq,aexpr)))

    

    let rec verification_conditions: List<Predicate> =
        match C with
            |Do(inv,gc) ->
                match gc with 
                    | Guard(b,c) ->
                        [BooleanOp(BooleanOp(inv,LAnd,Not(b)),Implies,Q)] @ [BooleanOp(P,Implies,inv)] @ [BooleanOp(sPC(C,BooleanOp(b,LAnd,P),varPrime 0,0),Implies,Q)]@[] 
                    |_ -> failwith "g"
            |_ ->   [BooleanOp(sPC(C,P,varPrime 0,0),Implies,Q)]@[]
      
    // Let this line stay as it is.
    { verification_conditions = List.map serialize_predicate verification_conditions}
