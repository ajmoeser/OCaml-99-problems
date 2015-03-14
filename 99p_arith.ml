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


