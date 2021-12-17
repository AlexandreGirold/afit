(** Factoring Builtin int primes *)
(*#use "builtin.ml"*)
open Builtin
open Basic_arithmetics

(** Factors product of two primes.
    @param key is public key of an RSA cryptosystem.
 *)
let break key =
  let (n,_)= key in
  if  n mod 2 = 0
  then (2,quot n 2)
  else
    let rec aux c =
      if  n mod c =0
      then (c,quot n c)
      else aux (c+2)
  in aux 3;;

