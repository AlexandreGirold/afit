(** Testing for primality *)

open Builtin
open Basic_arithmetics
open Power

(** Deterministic primality test *)

let is_prime n = let rec prime n i =
                   if n mod i =0 then false
                   else if (i*i)>n then true
                   else prime n (i+1)
                 in prime n 2;;
                 
(** Primality test based on small Fermat theorem
    @param p tested integer
    @param testSeq sequence of integers against which to test
 *)
let is_pseudo_prime p test_seq = true
