(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup tup = let (a, b, c) = tup in (c, b, a);;

let is_odd x = 
    if x mod 2 <> 0 then true
    else false;;

let area x y = 
    match x,y with 
        (x1, y1),(x2, y2) -> abs ((x1 - x2)*(y1 - y2));;

let volume x y = 
    match x,y with 
        (x1,y1,z1),(x2,y2,z2) -> abs ((x1 - x2)*(y1 - y2)*(z1 - z2));;

(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec fibonacci n = 
    if n < 1 then 0
    else if n = 1 then 1
    else (fibonacci (n-1)) + (fibonacci (n-2));;

let rec pow x y = 
    if y = 0 then 1
    else x * pow x (y-1);;

let rec log x y = 
    if x > y then 0
    else 1 + log x (y/x);;

let rec gcf x y = 
    if y = 0 then x
    else gcf y (x mod y);;

let rec is_prime_rec x y =
    if y * y > x then true
    else if x mod y == 0 then false
    else is_prime_rec x (y + 2);;

let rec is_prime x = 
    if(x <= 2 || x mod 2 == 0) then (x == 2)
    else is_prime_rec x 3;;

(*****************)
(* Part 3: Lists *)
(*****************)

let rec get idx lst = 
    match lst with
            |[] -> failwith "Out of bounds"
            |h::t ->
                if idx = 0 then h
                else (get (idx-1) t);;


let rec length_c lst =
    match lst with
            |[] -> 0
            |h::t -> 1 + length_c t;;


let larger lst1 lst2 = 
    if length_c lst1 > length_c lst2 then lst1
    else if length_c lst2 > length_c lst1 then lst2
    else [];;

let rec reverse_rec (lst, lst2)= 
    match lst with
        |[] -> lst2
        |h::t -> reverse_rec(t, (h::lst2));;

let reverse lst = reverse_rec (lst, []);;

let rec combine lst1 lst2 = 
    match lst1 with
        |h::t -> h::combine t lst2
        |[] -> lst2;;

let rec merge lst1 lst2 = 
    match lst1, lst2 with
        |[],[] -> []
        |h::t,[] -> lst1
        |[],h::t -> lst2
        |h1::t1,h2::t2 -> 
            if h1 > h2 then h2::(merge lst1 t2) 
            else h1::(merge t1 lst2);;

let rec rotate offset lst = 
    if offset == 0 then lst
    else
        match lst with
            |[] -> []
            |[a] -> [a]
            |h::t -> (rotate (offset - 1) (combine t (h::[])));;

let rec is_palindrome lst = lst = reverse lst;;

