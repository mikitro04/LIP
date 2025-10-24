type expr =
    True
    | False
    | Not of expr
    | And of expr * expr
    | Or of expr * expr
    | If of expr * expr * expr
    | Zero
    | Succ of expr
    | Pred of expr
    | IsZero of expr

type exprval = Bool of bool | Nat of int

let rec eval (e : expr) : exprval = 
    match e with
    | Zero -> Nat (0)
    | Succ(x) ->  
    | Pred(x) ->  
    | IsZero(x) ->  
    | IsZero(x) ->  
    | _ -> failwith("error")
;;