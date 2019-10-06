open Funs

(***********************)
(* Part 2: Integer BST *)
(***********************)

type int_tree =
  | IntLeaf
  | IntNode of int_tree * int_tree * int

let empty_int_tree = IntLeaf

let rec int_insert x t =
  match t with
  | IntLeaf -> IntNode(IntLeaf, IntLeaf, x)
  | IntNode (l, r, y) when x > y -> IntNode (l, int_insert x r, y)
  | IntNode (l, r, y) when x = y -> t
  | IntNode (l, r, y) -> IntNode (int_insert x l, r, y)

let rec int_mem x t =
  match t with
  | IntLeaf -> false
  | IntNode (l, r, y) when x > y -> int_mem x r
  | IntNode (l, r, y) when x = y -> true
  | IntNode (l, r, y) -> int_mem x l

(* Implement the functions below. *)

let rec int_size t = match t with
  | IntLeaf -> 0
  | IntNode (l, r ,y) -> 1 + int_size l + int_size r
;;

let rec int_max t = match t with
  | IntLeaf -> invalid_arg "int_max"
  | IntNode (l, r, y) when r = IntLeaf -> y
  | IntNode (l, r, y) -> int_max r
;;

let rec int_insert_all lst t =
  fold (fun a b -> int_insert b a) t lst
;;

let rec ial_helper t a = match t with
  | IntLeaf -> a
  | IntNode (l,r,y) ->
    let right = ial_helper r a in
    let node = y::right in
    ial_helper l node
;;

let int_as_list t =
    ial_helper t []
;;

(***************************)
(* Part 3: Polymorphic BST *)
(***************************)

type 'a atree =
    Leaf
  | Node of 'a * 'a atree * 'a atree
type 'a compfn = 'a -> 'a -> int
type 'a ptree = 'a compfn * 'a atree

let empty_ptree f : 'a ptree = (f,Leaf)

(* Implement the functions below. *)

let rec pinsert_helper x t f = match t with
  | Leaf -> Node(x, Leaf, Leaf)
  | Node (y, l, r) when f x y > 0 -> Node (y, l, pinsert_helper x r f)
  | Node (y, l, r) when f x y = 0 -> t
  | Node (y, l, r) -> Node (y, pinsert_helper x l f, r)
;;

let rec pinsert x t = match t with
  | (f, tree) -> (f, pinsert_helper x tree f)
;;

let rec pmem_helper x t f = match t with
  | Leaf -> false
  | Node (y, l, r) when f x y > 0 -> pmem_helper x r f
  | Node (y, l, r) when f x y = 0 -> true
  | Node (y, l, r) -> pmem_helper x l f
;;

let pmem x t = match t with
  | (f, tree) -> pmem_helper x tree f
;;

let rec pia_helper lst tree f =
  fold (fun a b -> pinsert b a) (f,tree) lst
;;

let pinsert_all lst t = match t with
  | (f, tree) -> pia_helper lst tree f
;;

let rec pial_helper t a = match t with
  | Leaf -> a
  | Node (y,l,r) ->
    let right = pial_helper r a in
    let node = y::right in
    pial_helper l node
;;

let rec p_as_list t = match t with
  | (f, tree) -> pial_helper tree []
;;

let pmap f t = match t with
  | (fn, tree) -> pinsert_all (map f (p_as_list t)) (fn,Leaf)
;;

(*******************************)
(* Part 4: Graphs with Records *)
(*******************************)

type node = int
type edge = { src : node; dst : node; }
type graph = { nodes : int_tree; edges : edge list; }

let empty_graph = {nodes = empty_int_tree; edges = [] }

let add_edge e { nodes = ns; edges = es } =
    let { src = s; dst = d } = e in
    let ns' = int_insert s ns in
    let ns'' = int_insert d ns' in
    let es' = e::es in
    { nodes = ns''; edges = es' }

let add_edges es g = fold (fun g e -> add_edge e g) g es

(* Implement the functions below. *)

let graph_empty g = match g with
  | {nodes = n; _} when n = empty_int_tree -> true
  | {nodes = n; _} -> false
;;

let graph_size g = match g with
  | {nodes = n; _} -> int_size n
;;

let is_dst n e = match e with
  | {src = s; dst = d} when d = n -> true
  | {src = s; dst = d} -> false
;;

let helper_helper e n = match e with
  | {src = s; dst = d} when s = n -> true
  | {src = s; dst = d} -> false
;;

let rec src_edges_helper el n = match el with
  | [] -> []
  | h::t -> if helper_helper h n then h::src_edges_helper t n else src_edges_helper t n
;;

let src_edges n g = match g with
  | {nodes = no; edges = el} -> rev(src_edges_helper el n)
;;

let rec reachable_helper el n tree oel = match el with
  | [] -> tree
  | {src = s; dst = d}::t when s = n -> if int_mem d tree
    then reachable_helper t n tree oel else
      reachable_helper t n (reachable_helper oel d (int_insert_all [d;n] tree) oel) oel
  | {src = s; dst = d}::t when d = n -> (reachable_helper t n (int_insert n tree) oel)
  | {src = s; dst = d}::t -> reachable_helper t n tree oel
;;

let reachable n g =  match g with
| {nodes = no; edges = el} -> reachable_helper el n IntLeaf el
;;
