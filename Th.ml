(*02/10/2025*)
type boolExpr = 
    True 
    | False 
    | If of boolExpr * boolExpr * boolExpr
    | And of boolExpr * boolExpr
    | Or of boolExpr * boolExpr
    | Not of boolExpr
    | Implies of boolExpr * boolExpr
;;

let rec eval (b : boolExpr) : bool =
    match b with
    | True -> true
    | False -> false
    | If (b1, b2, b3) -> if eval b1 then eval b2 else eval b3
    | And (b1, b2) -> (eval b1) && (eval b2)
    | Or (b1, b2) -> (eval b1) || (eval b2)
    | Not (b1) -> not(eval b1)
    | Implies (b1, b2) -> not(eval b1) || (eval b2)
 ;;
(*Firsts asserts*)
assert(eval True = true);;
assert(eval False = false);;
assert(eval (If (True, True, False)) = true);;
assert(eval (If (False, True, False)) = false);;
assert(eval (If (False, False, True)) = true);;

(*AND*)
assert(eval (And (False, False)) = false);;
assert(eval (And (False, True)) = false);;
assert(eval (And (True, False)) = false);;
assert(eval (And (True, True)) = true);;
(*OR*)
assert(eval (Or (False, False)) = false);;
assert(eval (Or (False, True)) = true);;
assert(eval (Or (True, False)) = true);;
assert(eval (Or (True, True)) = true);;
(*NOT*)
assert(eval (Not (True)) = false);;
assert(eval (Not (False)) = true);;
(*IMPLIES*)
assert(eval (Implies (False, False)) = true);;
assert(eval (Implies (False, True)) = true);;
assert(eval (Implies (True, False)) = false);;
assert(eval (Implies (True, True)) = true);;

(*--------------------------------------------------------------------------------------------------------------------------------------*)
(*16/10/2025*)


