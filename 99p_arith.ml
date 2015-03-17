(* arithmetic problems from http://ocaml.org/learn/tutorials/99problems.html#Arithmetic *)

(* checks whether given integer is prime *)
let is_prime (num : int) : bool =
  let rec check n i =
    if (i * i) >  abs n then true
    else if n mod i = 0 then false
    else check n (i + 1) in
  check num 2
;;


(* uses Euclid's algorithm to calculate greatest common
 * divisor *)
let rec gcd (x : int) (y : int) : int =
  if y = 0 then x
  else gcd y  (x mod y)
;;


(* determines whether two ints are coprime *)
let coprime (x : int) (y : int) : bool =
  (gcd x y) = 1
;;

(* Euler's totient function (primitive) *)
let phi (n : int) : int =
  let rec count_coprimes x =
    if x = 1 then 1
    else if (gcd x n = 1) then 1 + count_coprimes (x - 1) 
    else count_coprimes (x - 1) in
  count_coprimes n
;;


(* lists prime factors of given positive int in ascending order *)
let factors (n : int) : int list =
  let rec aux num fact acc =
    if fact > num then acc
    else if num mod fact = 0 then aux (num / fact) fact (fact::acc)
    else aux num (fact + 1) acc in
  List.rev (aux n 2 [])
;;


(* lists all prime numbers between lower and upper bound inclusive *)
let all_primes (lower : int) (upper : int) : int list =
  let rec aux l u acc =
    if l > u then acc
    else if is_prime l then aux (l + 1) u (l::acc)
    else aux (l + 1) u acc in
  List.rev (aux lower upper [])
;;
