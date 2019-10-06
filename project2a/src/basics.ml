(******************************)
(* Part 1: Simple Functions   *)
(******************************)

let dup a b c =
  if a = b || a = c || b = c then
    true
  else
    false
;;

let head_divisor lst = match lst with
  | [] -> false
  | a :: [] -> false
  | a :: b :: _ -> if (b mod a) = 0 then true else false
;;

let second_element lst = match lst with
    | [] -> -1
    | a :: [] -> -1
    | a :: b :: _ -> b
;;

let max_first_three lst = match lst with
    | [] -> -1
    | a :: [] -> a
    | a :: b :: [] -> if a > b then a else b
    | a :: b :: c :: _ -> if a >= b && a >= c then a
      else if b >= a && b >= c then b else c
;;

(*********************************)
(* Part 2: Recursive Functions   *)
(*********************************)

let rec cubes n =
  if n <= 0 then 0
  else (n*n*n) + cubes(n-1)
;;

let rec sum_odd lst = match lst with
  | [] -> 0
  | a :: b -> if a mod 2 = 1 || a mod 2 = -1 then a + sum_odd b else 0 + sum_odd b
;;

let rec is_even_sum lst = match lst with
  | [] -> true
  | a :: b -> if (a mod 2) = 0 then is_even_sum b else not(is_even_sum b)
;;

let rec count_occ lst target = match lst with
  | [] -> 0
  | a :: b -> if a = target then 1 + count_occ b target else count_occ b target
;;

let rec dup_list_helper m lst = match lst with
  | [] -> false
  | a :: b -> if a = m then true else dup_list_helper m b
;;
let rec dup_list lst = match lst with
  | [] -> false
  | a :: b -> dup_list_helper a b || dup_list b
;;
(****************)
(* Part 3: Sets *)
(****************)

let rec elem x a = match a with
  | [] -> false
  | h :: t -> if h = x then true else elem x t
;;

let rec insert_helper x a = match a with
  | [] -> [(x)]
  | (h) :: t -> (h) :: insert_helper (x) t
;;

let insert x a =
  if elem x a then a else insert_helper x a
;;

let rec subset a b = match a with
  | [] -> true
  | h :: t -> if elem h b then true && subset t b else false
;;

let rec length l = match l with
  | [] -> 0
  | (_::t) -> 1 + (length t)
;;

let eq a b =
  if length a = length b && subset a b then true else false
;;

let rec remove x a = match a with
  | [] -> []
  | h :: t -> if h = x then remove x t else h :: remove x t
;;

let rec union a b = match a with
  | [] -> b
  | h :: t -> if elem h b then union t b else union t (insert h b)
;;

let rec diff a b = match a with
  | [] -> []
  | h :: t -> if elem h b then diff t b else h :: diff t b
;;

let rec cat x a = match a with
  | [] -> []
  | h :: t -> (x , h) :: cat x t
;;
