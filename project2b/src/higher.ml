open Funs

(********************************)
(* Part 1: High Order Functions *)
(********************************)

let len lst =
  fold (fun a _ -> a+1) 0 lst
;;

let count_greater lst target =
  fold (fun a b -> if b > target then a + 1 else a) 0 lst
;;

let greater_tuple lst =
  map (fun a -> (a, count_greater lst a)) lst
;;

let flat_pair lst =
  rev (fold (fun a (b,c) -> c::b::a) [] lst)
;;

let rm lst target =
  rev(fold (fun a b -> if b <= target then b::a else a) [] lst)
;;


let rec sum xs ys =
  let al = fold(fun a (e,m) -> (e, (m+(mult(ys e)))) [] xs in
  let b = fold(fun a (e,m) -> let r = (e, (m+(mult(xs e)))) in
                if List.mem r al then a else r::a)[] ys in al@b
;;
