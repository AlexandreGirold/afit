(** Basic arithmetics with builtin integers *)
#mod_use "builtin.ml";;
open Builtin

(** Greater common (positive) divisor of two non-zero integers.
    @param a non-zero integer
    @param b non-zero in
 *)

let rec gcd a b = match (a, b) with
  |(0,b)-> b
  |(a,0)-> a
  |(a,b) when a=b -> a
  |(a,b) when a>b -> gcd (a-b) b
  |_-> gcd a (b-a);;

gcd 36 18;;

(** Extended euclidean division of two integers NOT OCAML DEFAULT.
    Given non-zero entries a b computes triple (u, v, d) such that
    a*u + b*v = d and d is gcd of a and b.
    @param a non-zero integer
    @param b non-zero integer.
*)
let bezout a b =
  let rec u 

