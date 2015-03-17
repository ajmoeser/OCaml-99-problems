(* Ocaml practice using the 99 problems from Ocaml.org *)

(* lists for testing *)
let e = [];;
let intl = [1;2;3;4;5;6;7;8;9;10];;
let single = [1];;
let charl = ["a";"a";"b";"c";"c";"c";"c";"d"];;
let rle = [(3,"a");(1,"b");(2,"c")];;

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
  let rec aux (xs : 'a list) (acc1 : 'a list) (acc2 : 'a list list) : 'a list list =
    match xs with
    | [] -> acc2
    | a::b::tl -> if a = b then aux (b::tl) (a::acc1) acc2
      else aux (b::tl) [] ((a::acc1)::acc2)
    | [a] -> (a::acc1)::acc2 in
  List.rev (aux lst [] [])
;;


(* run-length encoding *)
let encode (xs : 'a list) : (int * 'a) list =
  let rec aux lst count acc =
    match lst with
    | [] -> []
    | a::(b::_ as tl) -> 
      if a = b then aux tl (count + 1) acc
      else aux tl 0 ((count + 1,a)::acc)
    | [a] -> (count + 1, a)::acc in
  List.rev (aux xs 0 [])
;;

(* decode run-length encoding *)
let rec decode (ps : (int * 'a) list) : 'a list =
  match ps with 
  | [] -> []
  | (0,_)::tl -> decode tl
  | (n,x)::tl -> x::(decode ((n - 1,x)::tl)) 
;;


(* duplicate each element of list *)
let rec duplicate (xs : 'a list) : 'a list =
  match xs with
  | [] -> []
  | hd::tl -> hd::(hd::(duplicate tl))
;;

(* make n duplicates of each element of list *)
let replicate (xs : 'a list) (n : int) : 'a list =
  let rec aux lst i acc =
    match lst with
    | [] -> acc
    | hd::tl -> if i > 0 then aux lst (i - 1) (hd::acc)
      else aux tl n acc in
  List.rev (aux xs n [])
;;


(* drops every n'th element; indexed from 1 *)
let drop_nth (xs : 'a list) (n : int) : 'a list =
  let rec drop lst i acc =
    match lst with 
    | [] -> acc
    | hd::tl -> if i = n then drop tl 1 acc
      else drop tl (i + 1) (hd::acc) in
  List.rev (drop xs 1 [])
;;


(* splits list into two parts; length of first part given by n *)
let split (xs : 'a list) (n : int) : ('a list * 'a list) =
  let rec aux lst i acc =
    match lst with
    | [] -> (List.rev acc, [])
    | hd::tl as l -> if i > 0 then aux tl (i - 1) (hd::acc)
      else (List.rev acc,l) in
  aux xs n []
;;


(* extracts slice of lists, from i to k inclusive; indexed from 0 *)
let slice (xs : 'a list) (i : int) (k : int) =
  let rec get_slice lst n acc =
    match lst with
    | [] -> acc
    | hd::tl -> if n <= k then get_slice tl (n + 1) (hd::acc)
                else acc in
  List.rev (get_slice xs i [])
;;
