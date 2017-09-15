(* 

HOMEWORK 1

Name: Emma Price

Email: emma.price@students.olin.edu

Remarks, if any:
 *)



(* Question 1 *)


let rec expt (a:int) (b:int):int = if b>=2 then a * (expt a (b-1)) 
else (if b = 1 then a else failwith "exponent less than 1");;

let rec fac n = if n>1 then n * (fac (n-1)) else 1;;

let rec choose (n:int) (k:int):int = if n>1 then (if k>1 then (fac n)/(fac k * fac (n-k)) else n) else 1;;

let rec gcd (a:int) (b:int):int= if a<0||b<0 then failwith "positive integers required" else (if a<>b then (if a<>0 then (if b<>0 then (if a>=b then (gcd (a-b) b) else (gcd a (b-a))) else a) else b) else a);;

let rec coprimes a b = if a<=0 || b<=0 then [] else (if (gcd a b) = 1 then [b]@coprime a (b-1) else coprime a (b-1));;
let rec coprimes (n:int):int list = coprime n n;;

(* Question 2 *)

let rec tripleUp (xs:'a list):'a list = match xs with []->[] | head::tail -> head::head::head::(tripleUp tail);;

let rec nth (n:int) (xs:'a list):'a = match xs with []->failwith "empty list" | head::tail -> (if tail<>[] && n<1 then failwith "n is out of bounds" else (if n=0 then head else nth (n-1) tail));;

let rec last (xs:'a list):'a = match xs with []->failwith "empty list" | head::tail -> (if tail=[] then head else last tail);;

let rec appendAll (xss:'a list list):'a list= match xss with []->[] | head::tail -> head@(appendAll tail);;

let rec split_list xs list1 list2 = match xs with []->([],[]) | head::tail-> (if tail=[] then (head::list2,list1) else (split_list tail (head::list2) list1));;
let rec split (xs:'a list):('a list * 'a list) = split_list xs [] [];;

(* QUESTION 3 *)


let rec addV (v:int list) (w:int list):int list = match (v, w) with ([],[])->[] | (head1::tail1,[])->head1::tail1 | ([],head2::tail2)->head2::tail2 | (head1::tail1, head2::tail2)->[head1+head2]@(addV tail1 tail2);;

let rec scaleV (a:int) (v:int list):int list = match v with []->[] | head::tail -> [head*a]@(scaleV a tail);;

let rec inner (v:int list) (w:int list):int = match (v, w) with ([],[])->0 | (head1::tail1,[])->failwith "uneven list length" | ([],head2::tail2)->failwith "uneven list length" | (head1::tail1, head2::tail2)->(head1*head2)+(inner tail1 tail2);;

let rec outer (v:int list) (w:int list):int list list = match v with []->[] | head1::tail1 -> (match w with []->[] | head2::tail2 -> ([scaleV head1 w])@(outer tail1 w));;
