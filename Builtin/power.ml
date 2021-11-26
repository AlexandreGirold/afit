(** Power function implementations for builtin integers *)

open Builtin
open Basic_arithmetics

(** Naive power function. Linear complexity
    @param x base
    @param n exponent
 *)
let pow x n = let rec aux x n = match n with
                |0->1
                |n-> x*(aux x (n-1) )
              in aux x n;;


(** Fast integer exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
 *)
let rec power x n = if (x=1)||(n=0) then 1
                    else if n mod 2 = 0 then
                    power x (n/2)*power x (n/2)
                    else power x (n-1)*x;;


(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
    @param m modular base
 *)

let mod_power x n m =
  let rec mod_power_rec x n m =
    let b = if n = 0
            then 1
            else let a = mod_power_rec x (n/2) m
                 in if n mod 2 = 0
                    then (a*a) mod m
                    else (((a*a) mod m ) * x) mod m in
    if x < 0 && n mod 2 = 1
    then b + m
    else b
  in mod_power_rec x n m;;


(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base
    @param n exponent
    @param p prime modular base
 *)

 let prime_mod_power x n p = 5;;
   if n<p then mod_pow x n p
   else
     let r= n mod p
                    
                    
 
