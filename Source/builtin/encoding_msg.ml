(** Encoding Strings *)
(*#use "power.ml";;*)

open Builtin
open Basic_arithmetics
open Power



(** Encode a string containing ASCII characters.
    @param str is a string representing message.
    @param bits number of bits on which to store a character ; alphanumeric ASCII is 7.
 *)
let encode str bits =
  let l = String.length(str)-1 in
  let rec aux i s_int n = match i with
    |i when i>=0-> aux (i-1) (Char.code(str.[i])*power 2 n+s_int) (n+bits)
    |_-> s_int in 
  aux l 0 0;;




(** Decode a string containing ASCII characters.
    @param msg is an integer representing an encoded message.
    @param bits number of bits on which to store a character ; alphanumeric ASCII is 7.
 *)
let decode msg bits =
  let rec aux d msg = match msg with
    |0->""
    |msg-> aux d (quot msg d) ^ Char.escaped (Char.chr(modulo msg d))
    
  in
  aux (power 2 bits) msg;;


