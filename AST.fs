// This file implements a module where we define a data type "expr"
// to store represent arithmetic expressions
module AST

type expr =
    | Num2 of float
    | TimesExpr of (expr * expr)
    | DivExpr of (expr * expr)
    | PlusExpr of (expr * expr)
    | MinusExpr of (expr * expr)
    | PowExpr of (expr * expr)
    | UPlusExpr of (expr)
    | UMinusExpr of (expr)


//C  ::=  x := a  |  A[a] := a  |  skip  |  C ; C  |  if GC fi  |  do GC od
//GC ::=  b -> C  |  GC [] GC
//a  ::=  n  |  x  |  A[a]  |  a + a  |  a - a  |  a * a  |  a / a  |  - a  |  a ^ a  |  (a)
//b  ::=  true  |  false  |  b & b  |  b | b  |  b && b  |  b || b  |  ! b
//     |  a = a  |  a != a  |  a > a  |  a >= a  |  a < a  |  a <= a  |  (b)

type var = 
    | Var of string
    | Array of (string * aexp)


and cexp = 
    | Assign of (var * aexp)
    | Skip
    | C of (cexp * cexp)
    | If of (gcexp)
    | Do of (gcexp)


and aexp = 
  | Num of (float)
  | V of var
  | Plus of (aexp * aexp)
  | Minus of (aexp * aexp)
  | Mult of (aexp * aexp)
  | Div of (aexp * aexp)
  | UMinus of (aexp)
  | Pow of (aexp * aexp)


and bexp =
  | True
  | False
  | SAnd of (bexp * bexp)
  | SOr of (bexp * bexp)
  | And of (bexp * bexp)
  | Or of (bexp * bexp) 
  | Not of bexp
  | Eq of (aexp * aexp)
  | Neq of (aexp * aexp)
  | Gt of (aexp * aexp)
  | Geq of (aexp * aexp)
  | Lt of (aexp * aexp)
  | Leq of (aexp * aexp)


and gcexp = 
  | Then of (bexp * cexp)
  | GC of (gcexp * gcexp)
