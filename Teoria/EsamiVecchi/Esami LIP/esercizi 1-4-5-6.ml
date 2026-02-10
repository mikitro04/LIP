let foo a f b = if(f a = b )then a else a;;
'a -> ('a -> 'b) -> 'b -> 'a

foo: (`a >string) >`a > stringSoluzione: let foo g b = g b^"a" ;;

int → string → string → stringlet f x a b = if (x = 1) then a^" " else b;;

string * string → int → intlet f (a, b) n = if (a^b = “”) then n+1 else n-1;;

int * int → float * floatlet f (n,m) = if (n + m = 1) then (2.0 , 3.0)else (2.0, 3.0);;

float → int → floatlet rec f m n = if ( n = 0 ) then 2.0 else f 1.0 (n-1) ;;8. Definire le seguenti funzioni di base, su interi

string * string → int → intlet f (a, b) n = if (a^b = “”) then n+1 else n-1;;


1.1 ('a -> 'b) -> 'a list -> 'b list
1.2 int list -> int list
1.3 ('a -> bool) -> 'a list -> 'a list
1.4 (int * float) list -> (string * string) list
1.5 ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
1.6 ('a * 'b) list -> 'a list * 'b list *)

(*1.1*) List.map;;
(*1.2*)let foo l = List.map (fun x -> x +2) l;;
(*1.3*)List.filter;;
(*1.4*) let foo l = List.map (fun x -> if (fst x = 0 && snd x = 2.0 ) then ("Anna","Paolo")    
	else ("Anna","Paolo") ) l ;;
(*1.5*)List.fold_right;;
(*1.6 pair *) let pair l = List.fold_right (  fun (a,b) (l1,l2) -> (a::l1, b::l2)   ) l ([],[]) ;;

fool [1;2;3;4] [5;6;7] 5;;
- : (int * int) list = [(1, 5); (2, 6); (3, 7)] *)


let rec fool l1 l2 n = if(n == 0) then [] else match l1,l2 with
  [],[] -> []
|[],_ |_,[] -> []
|((hd1::tl1),(hd2::tl2)) -> (hd1,hd2)::(fool tl1 tl2 (n-1));;
fool [1;2;3;4] [5;6;7] 6;;
'a list -> 'b list -> int -> ('a * 'b) list

(*'a list -> 'b list -> ('a list * 'b list )  *)
let rec f l1 l2 = (l1@[], l2@[]);;

(*('a * 'b) list -> ('c * 'd) list) -> ('b * 'a) list * ('d * 'c) list    *)
let f l1 l2 = 
  (List.fold_right(fun (x, y) z -> (y, x)::z ) l1 [],
   List.fold_right(fun (x, y) z -> (y, x)::z ) l2 []);;
f[(1,2);(3,4)][("a",4);("b",5)];;


(* 'a list -> 'b list -> ('a * 'b list ) *)
let rec f l1 l2 = match (l1,l2) with
    [],[] -> []
  | _,[] | [],_ -> failwith "Lunghezze diverse"
  | h1::t1, h2::t2 -> (h1,h2)::(f t1 t2);;


'a >'b >'b *)let f x y = y;;

'a >int >'a *)let f x y = if y = 1 then a else a;;

(string * string > string) > string *)let f g = if g ("a","b") = "c" then "c" else "d";;

('a >'b) >('c >'a) >'c >'b *)let f g h x = g ( h x );;

('a >'b >'c) >'a >'b >'c *)let f g h x = g h x ;;

int * string >(int *string >int) >(int * string >int) >int*)
let f (a,b) g h = if b = “b” && a = 1 then g (a,b) + h (a,b);;

int * int > (int >float ) >float * float *)
let f (a,b) g = ( g (a+b) +.1. , 2.);;


'a list > int
List.length;;

'a list >'a list(*1b*) let f l = l @[];;

int list >int list(*1c*) let f l = 3:: l;;

'a list >('a >'b) >'b list(*1d*) let f l g = List.map g l;;

int list >(int >float) > float list(*1.5*) let f l g = 1.0:: (List.map g (1::l));;

('a >bool) >'a list >'a list(*1.6*) List.filter ;;

'a list >('a >bool) >('a >bool) >'a list(*1.7*) let f l p q = (List.filter p l) @(List.filter q l) ;;

'a >'b list >('b >'a >'a) >'a(*1.8*) let f z l g = List.fold_right g l z;;


foo: ('a -> int) -> 'a -> int -> 'a
let foo f a x = if (f a)+1 = x then a else a;;


foo: 'a -> int -> int -> 'a
let foo a x y = if ( x + y  < 0  ) then a else a;;


foo: (int -> 'a) -> 'a -> int
let foo f a = if ( f 1 ) = a then 1 else 2;;




 tipologia 4(************)
(******** Es1 n1-2013 Scrivere una funzione foo il seguente tipo: 
foo 'a list -> 'b list -> 'a list * 'b list **)
let foo l1 l2 = l1@[],l2@[];;

(****** Es1 n2-2013 Scrivere una funzione foo il seguente tipo: 
foo 'a list -> 'b list -> ('a * 'b) list *)
let rec foo l1 l2 = match l1,l2 with
  |[],[] -> []
  |hd1::tl1,hd2::tl2 ->  foo tl1 tl2 @ [hd1,hd2] ;;

(******* Es1 2014 Scrivere una funzione fooon il seguente tipo: 
foo ('a * 'b) list -> ('c * 'd) list ->('b * 'a) list * ('d * 'c) list *)
let foo a b = match (a,b) with
    |hd1::tl1,hd2::tl2 -> [snd hd1, fst hd1] , [snd hd2, fst hd2]
  |_ -> failwith "non va bene";; 

(*******1.1
 'a list -> 'b list -> ('a * 'b list) **)
let f a b = (List.nth a 1, b@[]);;

(******* 1.2
 'a list -> 'b list -> ('a list * 'b list) **)
let f a b = (a@[], b@[]);;

(******* 1.3 
('a list * 'b list) -> ('c list * 'd list) -> ('b * 'a) list * ('d * 'c) list  **)
let f a b = ((snd a@[], fst a@[]),(snd b@[], fst b@[])) ;;
let f ab cd = (List.nth (snd ab) 1,List.nth (fst ab) 1) :: [],
              (List.nth (snd cd) 1,List.nth (fst cd) 1) :: [];;

(*Scrivere una funzione foo con il seguente tipo:
foo: 'a list -> 'b list -> ('a -> 'b) list*)
let foo l1 l2 = match l1,l2 with
  |[],[] -> []
  |hd1::tl1,hd2::tl2 -> [if (f hd1) = hd2 then f hd1 :: foo tl1 tl2 else foo tl1 tl2] ;;

(* ('a -> 'b) -> 'a list -> 'b list *)
let rec foo f a = match a with
  |[] -> []
  |hd::tl -> (f hd) :: foo f tl;;







(*Parziale 2 n1 - 2013*)
(*Es1 - Scrivere una funzione foo con il seguente tipo:
  foo: 'a list -> b' list -> a' list * 'b list  *)
let foo a b = match (a,b) with
    ([],[]) -> ([],[])
  | (tl1, tl2) -> (tl1, tl2);;

(*Es2 - Definire una funzione fool con tipo: 
 fool: 'a list -> int -> 'a -> a' list      
  in modo che fool l n v sia la lista ottenuta da l rimpiazzando gli ultimi n elementi con v, e lasciando inalterati i rimanenti. 
  Ad esempio:  fool [1;2;3;4;5;6;7;8;9;10] 3 0  = [1;2;3;4;5;6;7;0;0;0]*)
let rec fool l n v = match l with       (*primi n elementi*)
    [] -> []
  | x::xs when (List.length xs) = (n-1) -> v::(fool xs (n-1) v)
  | x::xs -> x::(fool xs n v);;
   (*oppure*)
let rec fool l n v = match l with     (*primi n elementi*)
    [] -> []
  | hd::tl -> if n>0 then v::(fool tl (n-1) v) else hd::(fool tl n v) ;;
   (*oppure*)
let fool l n v = List.fold_right (fun a b -> if List.length b < n then [v]@b else [a]@b) l [];;
fool [1;2;3;4;5;6;7;8;9;10] 3 0;;

(*Es3 - Completare la seguente definizione della funzione foof con tipo: 
  'a list -> 'a -> 'a -> 'a list * a' list * a' list  
  let foof = List.fold_right... in modo che foof l a b sia la tripla di liste [l0, l1, l2] tali che 
      l0 contiene tutti gli elementi di l.     --Vuol dire tutti gli elementi minori di a--
      l1 contiene tutti gli elementi maggiori o uguali ad a e minori o uguali a b
      l2 contiene i rimanenti. 
  Ad esempio: foof [1;2;3;4;5;6;7;8;9;10] 3 7 = [1;2],[3;4;5;6;7],[8;9;10]  *)
let foof l a b = ((List.fold_right(fun x y -> if (x < a) then x::y else y) l []),
		  (List.fold_right(fun x y -> if (x >= a && x <= b) then x::y else y) l []),
		  (List.fold_right(fun x y -> if (x > b) then x::y else y) l [])
                 );;
foof [1;2;3;4;5;6;7;8;9;10] 3 7;;



(*Parziale 2 n2 - 2013*)
(*Es1 - Scrivere una funzione foo con il seguente tipo:
  foo: 'a list -> b' list -> ('a * 'b) list *)
let foo a b =
  if a = [] && b = [] then [(List.nth a 2), (List.nth b 2)]  (*List.nth serve per ottenere un solo elemento della lista che diventa 'a*)
  else [(List.nth a 2), (List.nth b 2)];;
  (*oppure*)
let foo a b = match (a,b) with
    (hd1::tl1,hd2::tl2) -> (hd1,hd2)::[]
  | _ -> [];;

(*Es2 - Definire una funzione fool con tipo:  
 fool: 'a list -> int -> 'a -> a' list      
  in modo che fool l n v sia la lista ottenuta da l rimpiazzando gli ultimi n elementi con v, e lasciando inalterati i rimanenti. 
  Ad esempio:  fool [1;2;3;4;5;6;7;8;9;10] 3 0  = [1;2;3;4;5;6;7;0;0;0]*)
let rec fool l n v = match (l,n) with     (*primi n elementi*)
    ([],_) -> []
  | (x::l , 0) -> v::(fool l n v)
  | (x::l , _) -> x::(fool l (n-1) v);;
(*oppure*)
let fool l n v = List.fold_right (fun a b -> if List.length b < n then [v]@b else [a]@b) l [];;
fool [1;2;3;4;5;6;7;8;9;10] 3 0;;

(*Es3 - Completare la seguente definizione della funzione foof con tipo: 
  'a list -> a' list  
  let foof = List.fold_right... in modo che foof l sia la lista ottenuta da l eliminando tutti gli elementi eccetto l'ultimo. Ad esempio:
  foof [1;2;3;4;5;6;7;8;9;10] = [10]  *)
let rec foof = function
    [] -> []
  | [x] -> [x]
  | x::xs -> (foof xs);;
foof [1;2;3;4;5;6;7;8;9;10];;
   (*oppure*)
let foof l = List.fold_right (fun a b -> if List.length b = 1 then b else a::b) l [];;
foof [1;2;3;4;5;6;7;8;9;10];;



(*Esercitazione compito*)
(*Es1 - Scrivere una funzione foo con il seguente tipo: 
 ('a * 'b) list -> ('c * 'd) list -> ('b * 'a) list * ('d * 'c) list   *)
let foo a b = match (a,b) with
    (hd1::tl1, hd2::tl2) -> ( ((snd hd1),(fst hd1))::[], ((snd hd2),(fst hd2))::[] ) 
  | _ -> ([],[]);;

(*Es2 - Definire una funzione fool con il seguente tipo: 
'a list -> 'b list -> int -> ('a * 'b) list
  in modo che fool l1 l2 n sia la lista ottenuta accoppiando i primi n elementi (se presenti)  di l1 e l2 . 
  Ad esempio:
  fool [1;2;3;4] [5;6;7] 2;;
  - : (int * int) list = [(1, 5); (2, 6)]
  fool [1;2;3;4] [5;6;7] 3;;
  - : (int * int) list = [(1, 5); (2, 6); (3, 7)] *)
let rec fool l1 l2 n = match (l1,l2) with
    (hd1::tl1, hd2::tl2) -> if n>0 then (hd1, hd2)::(fool tl1 tl2 (n-1)) else []
  | (_,_) -> [];;

(*Es3 - Completare la seguente definizione di funzione: 
  let foof l  = List.fold_right…
  'a list -> ('a * int) list
in modo che foof l  sia la lista di coppie  ottenuta accoppiando ogni elemento di l con la sua posizione nella lista. 
  Ad esempio, foof [10;20;30;40;50;60] =  [(10, 1); (20, 2); (30, 3); (40, 4); (50, 5); (60, 6)];;    *)
let foof l = List.fold_right (fun a b -> (a, List.length l - List.length b):: b) l [];;
foof [10;2;5;4] ;;

