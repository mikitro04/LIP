let foo f = f 1;;

let rec a n =
  if n=0 then 0
  else if b (n-1) mod 2 = 0 then a (n-1) + b (n-1)
  else b (n-1) + 1
and b n =
  if n=0 then 1
  else if a (n-1) mod 2 <> 0 then b (n-1) + 1
  else b (n-1);;
let seq n = (a n, b n);;

let fog f g x = if (g x) mod 2 = 0 then abs ((g x) - (f x)) else 2 * f (x+0);;
let comp f g = fog f g;;

let foo la1 lb la2 = match (la1,lb,la2) with
    (h1::t1,h2::t2,h3::t3) -> if h1=h3 then t2 else t2
  | _ -> failwith "fail";;

let rec fool l1 l2 n = match (l1,l2) with
    ([],[]) -> if n>0 then failwith "non ci sono abbastanza elementi!" else []
  | (hd::tl,[]) -> if n>0 then hd :: fool tl [] (n-1) else []
  | ([],hd::tl) -> if n>0 then hd :: fool [] tl (n-1) else []
  | (h1::t1,h2::t2) -> if n>1 then h1 :: h2 :: fool t1 t2 (n-2) else if n=1 then h1 :: fool t1 t2 (n-1) else [];;

let foo l1 l2 f = match (l1,l2) with
    (h1::t1,h2::t2) -> if (f h2)=h1 then [h1] else [h1]
  |  _ -> failwith "fail";;

type 'a tree = Empty | Node of 'a * 'a tree * 'a tree;;
let rec check t = match t with
    Empty -> true
  | Node(_,Empty,Node(_,_,_)) | Node(_,Node(_,_,_),Empty) -> false
  | Node(x,tl,tr) -> check tl && check tr;;

type boolean = One
             | Zero
             | And of boolean * boolean
             | Or of boolean * boolean
             | Implies of boolean * boolean
             | Equiv of boolean * boolean;;
let rec eval e = match e with
    One -> true | Zero -> false
  | And(e1,e2) -> (eval e1) && (eval e2)
  | Or(e1,e2) -> (eval e1) || (eval e2)
  | Implies(e1,e2) -> not(eval e1) || (eval e2)
  | Equiv(e1,e2) -> (eval e1) = eval (e2);;
