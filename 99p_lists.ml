(* Ocaml practice using the 99 problems from Ocaml.org *)

(* last element of a list *)
let rec last (xs : 'a list) : 'a option =
match xs with
|[] -> None
| [hd] -> Some hd
| _::tl -> last tl;;


(* last two elements of a list *)
let rec last_two (xs : 'a list) : ('a * 'a) option =
  match xs with 
  | [] | [_] -> None
  | [fst;scd] -> Some (fst,scd)
  | _::tl -> last_two tl;;


(* find the k'th element of a list; list indexed from one *)
let rec at (xs : 'a list) (k : int) : 'a option =
  if k < 1 then None else
  match xs with
  | [] -> None
  | hd::tl -> if k = 1 then Some hd else at tl (k - 1);;
  

(* finds length of list *)
let list_length (xs : 'a list) : int =
  let rec len lst i =
    match lst with
    | [] -> i
    | _::tl -> len tl (i + 1) in
  len xs 0;;


(* reverses a list *)
let list_rev (xs : 'a list) : 'a list =
  let rec rev lst acc =
    match lst with 
    | [] -> acc
    | hd::tl -> rev tl (hd::acc) in
  rev xs [];;


(* checks whether list is palindrome *)
let is_palindrome (xs : 'a list) : bool =
  xs = list_rev xs;;


(* flatten nested list structure *)
type 'a node =
| One of 'a
| Many of 'a node list;;

let flatten nodes =
  let rec flatTR lst acc =
    match lst with
    | [] -> acc
    | (One x)::tl -> flatTR tl (acc @ [x])
    | (Many ys)::tl -> flatTR tl ((flatTR ys acc)) in
  flatTR nodes [];;

(* test list for flatten *)
let l = [One 1; Many [One 2; Many [One 3;One 4];One 5];One 6];;


(* eliminate consecutive duplicates *)
let rec compress (xs : 'a list) : 'a list =
  match xs with
  | [] -> []
  | f::s::tl -> if f = s then (compress (s::tl))
		else f::(compress (s::tl))
  | f::[] -> [f]
;;


(* pack consecutive duplicates into sublists *)
let pack (lst : 'a list) : 'a list list =
  let rec aux (xs : 'a list) (acc : 'a list) : 'a list list =
    match xs with
    | [] -> []
    | f::s::tl -> if f = s then aux (f::tl) (s::acc)
      else  aux (s::tl) (f::acc)
    | [f] -> [f::acc] in
  aux lst []
;; (* NOT working *)


(* run-length encoding *)
(*let encode (xs : 'a list) : (int * 'a) list =
  let rec aux lst count =
*)

(* decode run-length encoding *)
let rec decode (ps : (int * 'a) list) : 'a list =
  match ps with 
  | [] -> []
  | (0,_)::tl -> decode tl
  | (n,x)::tl -> x::(decode ((n - 1,x)::tl)) 
;;

let rle = [(3,"a");(1,"b");(2,"c")];;


(* duplicate each element of list *)
let rec duplicate (xs : 'a list) : 'a list =
  match xs with
  | [] -> []
  | hd::tl -> hd::(hd::(duplicate tl))
;;


