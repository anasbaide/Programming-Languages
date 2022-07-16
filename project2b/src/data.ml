(*open Funs*)

(*************************************)
(* Part 2: Three-Way Search Tree *)
(*************************************)

type int_tree =
  | IntLeaf
  | IntNode of int * int option * int_tree * int_tree * int_tree ;;

let empty_int_tree = IntLeaf;;

let match_some x =
  match x with
  |None -> true
  |a -> false;;

let rec int_insert x t =
  match t with
  | IntLeaf -> IntNode(x, None, IntLeaf, IntLeaf, IntLeaf)
  | IntNode (a, b, c, d, e) when x = a -> t
  | IntNode (a, b, c, d, e) when a > x -> if match_some b then IntNode (x, Some a, c, d, e) else IntNode (a, b, int_insert x c, d, e)
  | IntNode (a, b, c, d, e) when a < x -> if match_some b then IntNode (a, Some x, c, d, e) else if b < Some x then IntNode (a, b, c, d, int_insert x e) else IntNode (a, b, c, int_insert x d, e)
  | IntNode (_, _, _, _, _) -> t;;

let rec int_mem x t =
  match t with
  | IntLeaf -> false
  | IntNode (a, b, c, d, e) when x = a -> true
  | IntNode (a, b, c, d, e) when a > x -> if match_some b then false else if Some x = b then true else int_mem x c
  | IntNode (a, b, c, d, e) when a < x -> if match_some b then false else if Some x = b then true else if b < Some x then int_mem x e else int_mem x d
  | IntNode (_, _, _, _, _) -> false;;

let rec int_size t =
  match t with
  | IntLeaf -> 0
  | IntNode (_, None, c, d, e) -> int_size c + 1
  | IntNode (_, _, c, d, e) -> int_size c + int_size d + int_size e + 2

let matcher x = 
match x with
  | Some v -> v + 0
  | None -> 0;;

let rec int_max t =
  match t with
  | IntLeaf -> raise (Invalid_argument("int_max"))
  | IntNode (a, None, c, d, e) -> if c = empty_int_tree then a else int_max c
  | IntNode (a, b, c, d, e) -> if e = empty_int_tree then matcher b else int_max e;;


(*******************************)
(* Part 3: Three-Way Search Tree-Based Map *)
(*******************************)

type 'a tree_map =
  | MapLeaf
  | MapNode of (int * 'a) * (int * 'a) option * 'a tree_map * 'a tree_map * 'a tree_map;;

let empty_tree_map = MapLeaf;;

let rec map_put k v t = 
  match t with
  | MapLeaf -> MapNode((k,v), None, MapLeaf, MapLeaf, MapLeaf)
  | MapNode ((a1,a2), b, c, d, e) when a1 = k -> raise (Invalid_argument("map_put"))
  | MapNode ((a1,a2), None, c, d, e) when a1 > k -> MapNode ((k,v), Some (a1,a2), c, d, e)
  | MapNode ((a1,a2), Some (b1, b2), c, d, e) when a1 > k -> MapNode ((a1,a2), Some (b1, b2), map_put k v c, d, e)
  | MapNode ((a1,a2), None, c, d, e) when a1 < k -> MapNode ((a1,a2), Some (k,v), c, d, e) 
  | MapNode ((a1,a2), Some (b1, b2), c, d, e) when b1 < k -> MapNode ((a1,a2),Some (b1,b2), c, d, map_put k v e) 
  | MapNode ((a1,a2), Some (b1, b2), c, d, e) when b1 > k ->  MapNode ((a1,a2),Some (b1,b2), c, map_put k v d, e)
  | MapNode (_, _, _, _, _) -> t;;

let rec map_contains k t = 
  match t with
  | MapLeaf -> false
  | MapNode ((a1,a2), b, c, d, e) when a1 = k -> true
  | MapNode ((a1,a2), None, c, d, e) when a1 > k -> false
  | MapNode ((a1,a2), Some (b1, b2), c, d, e) when a1 > k -> if b1 = k then true else map_contains k c
  | MapNode ((a1,a2), None, c, d, e) when a1 < k -> false
  | MapNode ((a1,a2), Some (b1, b2), c, d, e) when b1 < k -> map_contains k e 
  | MapNode ((a1,a2), Some (b1, b2), c, d, e) when b1 > k ->  map_contains k d
  | MapNode (_, _, _, _, _) -> false;;

let rec map_get k t =
  match t with
  | MapLeaf -> raise (Invalid_argument("map_get"))
  | MapNode ((a1,a2), None, c, d, e) when a1 = k -> a2
  | MapNode ((a1,a2), Some (b1,b2), c, d, e) when a1 = k -> a2
  | MapNode ((a1,a2), None, c, d, e) when a1 > k -> raise (Invalid_argument("map_get"))
  | MapNode ((a1,a2), Some (b1, b2), c, d, e) when a1 > k -> if b1 = k then b2 else map_get k c
  | MapNode ((a1,a2), None, c, d, e) when a1 < k -> raise (Invalid_argument("map_get"))
  | MapNode ((a1,a2), Some (b1, b2), c, d, e) when a1 < k -> if b1 = k then b2 else if b1 < k then map_get k e else map_get k d
  | MapNode (_, _, _, _, _) -> raise (Invalid_argument("map_get"));;


(***************************)
(* Part 4: Variable Lookup *)
(***************************)

(* Modify the next line to your intended type *)
type lookup_table = 
  | TableLeaf
  | TableNode of (string * int) list * lookup_table;;

let empty_table : lookup_table = TableLeaf;;

let push_scope (table : lookup_table) : lookup_table = TableNode ([], table);;

let pop_scope (table : lookup_table) : lookup_table =
  match table with
  |TableLeaf -> failwith "No scopes remain!"
  |TableNode(a, b) -> b;;

let rec table_mem lst name =
  match lst with
  |[] -> false
  |(n,v)::t -> if n = name then true else table_mem t name;;

let rec table_get lst name =
  match lst with
  |[] -> 0
  |(n,v)::t -> if n = name then v else table_get t name;;

let add_var name value (table : lookup_table) : lookup_table =
  match table with
  |TableLeaf -> failwith "There are no scopes to add a variable to!"
  |TableNode(lst, b) -> if table_mem lst name then failwith "Duplicate variable binding in scope!" else TableNode ((name, value)::lst, b);;

let rec lookup name (table : lookup_table) =
  match table with
  |TableLeaf -> failwith "Variable not found!"
  |TableNode(lst, b) -> if table_mem lst name then (table_get lst name) else lookup name b;;