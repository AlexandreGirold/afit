(** Testing for primality *)

(*#use "basic_arithmetics.ml";;*)
open Basic_arithmetics

(** Deterministic primality test **)

let is_prime n =
  if n=2 then true
  else
    let rec prime n i =
                   if n mod i =0 then false
                   else if (i*i)>n then true
                   else prime n (i+1)
                 in prime n 2;;


(** Primality test based on small Fermat theorem
    @param p tested integer
    @param testSeq sequence of integers against which to test
 *)




let is_pseudo_prime p test_seq = match p with
    | 2 -> true
    | _ ->
        let rec aux p test_seq = match test_seq with
            | [] -> true
            | e::l ->
                if gcd p e <> 1
                    then false
                else
                    aux p l
in aux p test_seq;;
