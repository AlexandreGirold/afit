(** Twerking OCaml builtin euclidean division

The OCaml built-in euclidian divisions operations do not follow the
standard mathematical conventions. We adapt OCaml available
primitives to suit maths conventions.

 **)

(** Sign function
    @param x integer
*)
let sign x = match x with
  |x when x<0 -> -1
  |_->1;;

sign (-15)

(** Quotient of an integer by a natural number.
    This is the quotient in euclidiant division sense.
    @param a dividend
    @param b natural number you divide by.
 *)
(*
let quot a b = if b=0 then
                 failwith "b must be diff from 0"
               else if abs(a) < abs(b)
               then 0
               else
                 let rec aux a b result =
                   if abs(a) < abs(b)
                   then result
                   else
                     if (a<0)&&(b<0)
                     then aux (abs(a)) (abs(b)) (result)
                     else
                       if (a<0)||(b<0) then aux (a+b) b (result-1)
                       else aux (a-b) b (result+1)

                 in aux a b 0;;
 *)

let quot a b =
  let quot= a/b in
  if a-b*quot<0 then
    quot-(sign b)
  else
    quot;;

(** Quotient of two integers. Fully Recursive.
    General case ; explicit by-hand computations. Not efficient enough as
    it is not a low level implementation.
*)

(** Modulo of two integers.
    Following euclidean division NOT OCAML DEFAULT. Positive integer
    between 0 (included) and modulo (excluded) resulting from euclidian
    division of entry by modulo.



    OCAML DEFAULT : For negative numbers eucldean result - modulo base.

    @param a input integer
    @param b moduli integer.
 *)

let modulo a b = a-b*(quot a b);;

(** Division of an integer by a natural number. NOT OCAML DEFAULT.
    Division of an integer by a non-zero integer b is the unique couple
    of integers (q, r) such that a = b*q + r and r is in [0, abs b[.
    @param a dividend
    @param b integer you divide by.
*)
(*
let div1 a b =
  let rec aux q r=
    if b=0 then invalid_arg "cannot devided by 0"
    else
      if r=0 || r<b
      then (q,r)
      else
        let t = r/b in
        aux (q+t)(r-t*b)
  in aux 0 a;;
 *)
let div a b  = (( quot a b ),( modulo a b));;

