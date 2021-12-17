(** Basic arithmetics with builtin integers *)
(*#use "builtin.ml";;*)
open Builtin

(** Greater common (positive) divisor of two non-zero integers.
    @param a non-zero integer
    @param b non-zero in
 *)

let signs x = match x with
  |x when x<0 -> -1*x
  |_->1*x;;


let rec gcd a b = match (a,b) with
  |(a,0)->signs(a)
  |(0,b)-> signs(b)
  |_-> gcd b  (a mod b);;
gcd (-18)(-12)

(** Extended euclidean division of two integers NOT OCAML DEFAULT.
    Given non-zero entries a b computes triple (u, v, d) such that
    a*u + b*v = d and d is gcd of a and b.
    @param a non-zero integer
    @param b non-zero integer.
 *)

let bezout a b =
  let rec aux a b =
    if a=0 then (0,1,b)
    else
      let q,r =div b a  in
      let (v,d,u) = aux r a in
      (d-q*v,v,u)
  in aux a b;;

