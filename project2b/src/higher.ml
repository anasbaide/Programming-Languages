open Funs

(********************************)
(* Part 1: High Order Functions *)
(********************************)

let contains_elem lst e = 
    fold (fun a h -> if h = e then true else a) false lst;;

let is_present lst x = 
    map (fun h -> if h = x then 1 else 0) lst;;

let count_occ lst target = 
    fold (fun a h -> if target = h then a + 1 else a) 0 lst;;

let func_help lst e = if contains_elem lst e then lst else e::lst;;

let uniq lst =
    rev (fold func_help [] lst);;

let assoc_list lst = 
    map (fun x -> (x, count_occ lst x)) (uniq lst);;

let append lst e =
  fold_right (fun t a -> t::a) lst e;;

let concat lst = fold_right (fun t a -> append t a) lst [];;

let ap fns args = 
    concat (map (fun fn -> map fn args) fns);;
