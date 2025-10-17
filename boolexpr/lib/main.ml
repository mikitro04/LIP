open Ast
exception NoRuleApplies

let parse (s : string) : boolExpr =
    let lexbuf = Lexing.from_string s in
    let ast = Parser.prog Lexer.read lexbuf in
    ast

(* ---BIG STEP SEMANTIC--- *)
let rec eval (x : boolExpr) : bool = 
    match x with
    | True -> true
    | False -> false
    | If (b1, b2, b3) -> if eval b1 then eval b2 else eval b3
    | Not (True) -> false
    | Not (False) -> true
    | And (True, e) -> eval e
    | And (e, True) -> eval e
    | And (_, _) -> false
    | Or (True, _) -> true
    | Or (_, True) -> true
    | Or (_, _) -> false
    | _ -> raise NoRuleApplies
;;


(* ---SMALL STEP SEMANTIC--- *)
let rec trace1 (e : boolExpr) : boolExpr = 
    match e with
    | If(True, t, _) -> t
    | If(False, _, f) -> f
    | If(c, t, f) -> If((trace1 c), t, f)

    | Not (True) -> False
    | Not (False) -> True

    | And (True, v) -> v
    | And (v, True) -> v
    | And (False, _) -> False
    | And (_, False) -> False
    | And (v1, v2) -> And(trace1 v1, trace1 v2)

    | Or (True, _) -> True
    | Or (_, True) -> True
    | Or (False, False) -> False
    | Or (v1, v2) -> Or(trace1 v1, trace1 v2)
    
    | _ -> raise NoRuleApplies
;;

let rec trace e = try
    let e' = trace1 e  
        in e::(trace e')
    with NoRuleApplies -> [e]
;;