(** A naive implementation of big integers

This module aims at creating a set of big integers naively. Such data
types will be subsequently called bitarrays. A bitarray is a list of
zeros and ones ; first integer representing the sign bit. In this
contexte zero is reprensented by the empty list []. The list is to
be read from left to right ; this is the opposite convention to the
one you usually write binary decompositions with. After the sign bit
the first encountered bit is the coefficient in front of two to
the power zero. This convention has been chosen to ease writing
down code.

 *)


let rec power x n = if (x=1)||(n=0) then 1
                    else if n mod 2 = 0 then
                    power x (n/2)*power x (n/2)
                    else power x (n-1)*x;;



(** Creates a bitarray from a built-in integer.
    @param x built-in integer.
*)
let from_int x = match x with
  |0->[0]
  |x when x<0 ->let rec aux1 q r l = match q with
         |0-> l
         |_ ->r:: (aux1 (q/2) (q mod 2) l)
       in 1::(aux1 ((x*(-1))/ 2) ((x*(-1)) mod 2) [1])
  |_-> let rec aux q r l = match q with
         |0-> l
         |_ ->r:: (aux (q/2) (q mod 2) l)
       in 0::(aux ( x/2) (x mod 2) [1]);;

from_int (0)

(** Transforms bitarray of built-in size to built-in integer.
    UNSAFE: possible integer overflow.
    @param bA bitarray object.
 *)
(*
let to_int bA = match bA with
  |f::b when f=1->
    let rec aux int count bA = match bA with
      |[]-> invalid_arg "not binary number given"
      |e::[] when e=1 -> if e=1
                then -(int+(power 2 count))
                else -int
      |e::b -> aux int (count +1) b
    in aux 0 0 bA
  |_->
    let rec aux int count bA = match bA with
      |[]-> invalid_arg "not binary number given"
      |e::[] -> if e=1
                then int+(power 2 count)
                else int
      |e::b -> if e=1
               then aux (int+(power 2 count)) (count+1) b
               else aux int (count +1) b
    in aux 0 0 bA;;
*)
let to_int bA =
  let rec aux bA i j res= match (bA, i) with
    |([], i) -> j*res
    |(e::bA, i) when i = (-1) -> aux bA (i+1) (if e=1 then (-1)  else (1)) res
    |(e::bA, i) when e = 0 -> aux bA (i+1) j res
    |(e::bA, i) -> aux bA (i+1) j (res + (power 2 i))
in aux bA (-1) 1 0;;



(** Prints bitarray as binary number on standard output.
    @param bA a bitarray.
  *)
let print_b bA = ()

(** Toplevel directive to use print_b as bitarray printer.
    CAREFUL: print_b is then list int printer.
    UNCOMMENT FOR TOPLEVEL USE.
*)
(* #install_printer print_b *)

(** Internal comparisons on bitarrays and naturals. Naturals in this
    context are understood as bitarrays missing a bit sign and thus
    assumed to be non-negative.
*)

(** Comparing naturals. Output is 1 if first argument is bigger than
    second -1 otherwise.
    @param nA A natural, a bitarray having no sign bit.
           Assumed non-negative.
    @param nB A natural.
 *)


let rec compare_n nA nB =
  let rec aux b1 b2 res = match (b1, b2) with
    |([],[])-> res
    |([],f2::b2)-> (-1)
    |(f1::b1,[]) ->  1
    |(f1::b1,f2::b2) -> if f2>f1
                        then aux b1 b2 (-1)
                        else
                          if f2<f1
                          then aux b1 b2 1
                          else aux b1 b2 res
  in aux nA nB 0;;

(** Bigger inorder comparison operator on naturals. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>!) nA nB = match (compare_n nA nB) with
  |1-> true
  |_-> false;;


(** Smaller inorder comparison operator on naturals. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<!) nA nB = match compare_n nA nB with
  |(-1) -> true
  |_-> false;;

(** Bigger or equal inorder comparison operator on naturals. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>=!) nA nB =match compare_n nA nB with
  |(-1) -> false
  |_-> true;;

(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<=!) nA nB =match compare_n nA nB with
  |(1) -> false
  |_-> true;;

(** Comparing two bitarrays. Output is 1 if first argument is bigger
    than second -1 otherwise.
    @param bA A bitarray.
    @param bB A bitarray.
 *)
(*
let compare_b bA bB =
  let rec aux b1 b2 count = match (b1, b2) with
    |([],[])-> 0
    |([],f2::b2)-> ( match f2 with
                 |1 -> (-1)
                 |_ -> 1)
    |(f1::b1,[]) ->  ( match f1 with
                 |1 -> 1
                 |_ -> -1)

    |(f1::b1,f2::b2) when count=0 ->( match f1-f2 with
                                      |(-1) -> 1
                                      |1 ->(-1)
                                      |_->aux b1 b2 (count+1))
    |(f1::b1,f2::b2) -> match f1-f2 with
                        |(-1) -> (-1)
                        |1 -> 1
                        |_-> aux b1 b2 1
  in aux bA bB 0;;
 *)
let compare_b bA bB =
  let compare_b_aux bA bB =
  match (bA,bB)with
    ([],[])->0

   |(e::l,[])-> if e=1
                then -1
                else 1

   |([],e::l)-> if e=0
                then -1
                else 1
   |(e::l),(e1::l1)->if e=e1
                     then
      match e with
      |0-> compare_n l l1
      |_->(compare_n l l1)*(-1)
                     else
                       if e<e1
                       then 1
                       else -1
  in compare_b_aux bA (bB);;

(** Bigger inorder comparison operator on bitarrays. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>) bA bB = match compare_b bA bB with
  |(1) -> true
  |_ -> false;;

(** Smaller inorder comparison operator on bitarrays. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<) bA bB = match compare_b bA bB with
  |(-1) -> true
  |_ -> false;;

(** Bigger or equal inorder comparison operator on bitarrays. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>=) bA bB = match compare_b bA bB with
  |(-1) -> false
  |_ -> true;;

(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<=) bA bB =  match compare_b bA bB with
  |(1) -> false
  |_ -> true;;


(** Sign of a bitarray.
    @param bA Bitarray.
*)
let sign_b bA = match bA with
  |[] -> invalid_arg "empty array"
  |e::l -> if e=1 then (-1)
           else 1;;


(** Absolute value of bitarray.
    @param bA Bitarray.
*)
let abs_b bA = match bA with
  |[] -> invalid_arg "empty array"
  |e::l -> l;;


(** Quotient of integers smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _quot_t a = if a < 2 then 0 else 1

(** Modulo of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _mod_t a = if a = 1 || a = 3 then 1 else 0

(** Division of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _div_t a = (_quot_t a, _mod_t a)

(** Addition of two naturals.
    @param nA Natural.
    @param nB Natural.
 *)


let add_n nA nB =
  let rec aux a b carry final = match (a,b) with
    |([],[]) -> carry::final
    |([],e2::l2) -> (match e2+carry with
                         |0 -> 0::(aux [] l2 0 final)
                         |1 -> 1::(aux [] l2 0 final)
                         |_ -> 0::(aux [] l2 1 final))
    |(e1::l1,[]) -> (match e1+carry with
                     |0 ->0::(aux l1 [] 0 final)
                     |1 -> 1::(aux l1 [] 0 final)
                     |_ ->0::(aux l1 [] 1 final))
    |(e1::l1,e2::l2) -> (match e1+e2+carry with
                         |0 -> 0::(aux l1 l2 0 final)
                         |1 -> 1::(aux l1 l2 0 final)
                         |2 -> 0::(aux l1 l2 1 final)
                         |_ -> 1::(aux l1 l2 1 final))
  in aux (clear nA) (clear nB) 0 [];;


(** Difference of two naturals.
    UNSAFE: First entry is assumed to be bigger than second.
    @param nA Natural.
    @param nB Natural.
*)
let diff_n nA nB =
  let rec aux a b carry final = match (a,b) with
    |([],[]) -> final
    |([],e2::l2) -> (match (carry+e2) with
                     |0 -> 0::(aux [] l2 0 final)
                     |(-1) -> 1::(aux [] l2 1 final)
                     |_ -> 0::(aux [] l2 1 final))
    |(e1::l1,[]) ->  (match e1-carry with
                      |0 -> 0::(aux l1 [] 0 final)
                      |(-1) -> 1::(aux l1 [] 1 final)
                      |_ -> 1::(aux l1 [] 0 final))
    |(e1::l1,e2::l2) -> match e1-(e2+carry) with
                        |0 -> 0::(aux l1 l2 0 final)
                        |(-1) -> 1::(aux l1 l2 1 final)
                        |1 -> 1::(aux l1 l2 0 final)
                        |_-> 0:: (aux l1 l2 1 final)
  in aux nA nB 0 [];;


(** Addition of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
 *)
let inverse bA =
  let rec aux f = function
    |[]-> f
    |e::l ->(aux (e::f) l)
  in aux [] bA;;


(*
let one bA =
  let rec aux f daddy = function
    |[] ->f
    |e::l when daddy=0-> e::(aux f (daddy + 1) l)
    |e::l -> if e=1 then 0::(aux f daddy l)
             else 1::(aux f daddy l )    
  in aux [] 0 bA;;
one [0;1;0;1]


sum [0;1;1;1;0][1;0;1;1]

let two bA =
  let rec aux f count = function
    |e::l when count=1 -> if e=1 then 1::(aux f (count+1) l)
                          else 0::(aux f count l)
    |e::l -> if e=1 then 0::(aux f count l)
             else 1::(aux f count l)
    |[]-> f
  in aux [] 1 (inverse bA);;

two [1;1;0;1]
 *)
 
let add_b bA bB = match (bA,bB) with
  |([],[]) -> []
  |([],bB) -> bB
  |(bA,[]) -> bA
  |(e1::l1,e2::l2) when e1=0 && e2=0 -> 0::add_n l1 l2
  |(e1::l1,e2::l2) when e1=1 && e2=1 -> 1::add_n l1 l2
  |(e1::l1,e2::l2) when e1=0 && e2=1 -> if ((>>!) l1 l2 = true)
                                        then 0::diff_n l1 l2
                                        else 1::diff_n l1 l2
  |(e1::l1,e2::l2)-> if ((>>!) l1 l2 = true)
                     then 1::diff_n l1 l2
                     else 0::diff_n l1 l2;;


(** Difference of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)
let diff_b bA bB =  match (bA,bB) with
  |([],[]) -> []
  |([],bB) -> bB
  |(bA,[]) -> bA
  |(e1::l1,e2::l2) when e1=0 && e2=0 -> if ((>>!) l1 l2 = true)
                                        then 0::diff_n l1 l2
                                        else 1::diff_n l2 l1
  |(e1::l1,e2::l2) when e1=1 && e2=1 -> if ((>>!) l1 l2 = true)
                                        then 1::diff_n l1 l2
                                        else 0::diff_n l2 l1
  |(e1::l1,e2::l2) when e1=0 && e2=1 -> 0::add_n l1 l2
  |(e1::l1,e2::l2) -> 1::add_n l1 l2;;


(** Shifts bitarray to the left by a given natural number.
    @param bA Bitarray.
    @param d Non-negative integer.
*)
let rec shift bA d = match (bA,d) with 
  |([],d)-> []
  |(l,0)-> l
  |(e::l,d) -> shift (e::0::l) (d-1);;

(** Multiplication of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
 *)

let mult_b bA bB =
  let rec aux bA bB res i j = match  (bA, bB, i) with
    |([], [], i) -> []
    |([], bB, i) -> [0] 
    |(bA, [], i) -> j::res
    |(a::l1, b::l2, 0) ->
      if (a = 0 && b = 0) || (a = 1 && b = 1)
      then aux l1 l2 res (i+1) 0
      else aux l1 l2 res (i+1) 1
    |(l1, e::l2, i) ->
      if e = 1
      then aux (shift l1 1) l2 (add_n res l1) i j
      else aux (shift l1 1) l2 res i j
  in aux bA bB [] 0 0;;


(** Quotient of two bitarrays.
    @param bA Bitarray you want to divide by second argument.
    @param bB Bitarray you divide by. Non-zero!
 *)

let  clear l = (*erase all the 0 before the first 1 starting at the rigth*)
  let rec aux l = match l with
    |[]->[]
    |a::l when a = 1 -> inverse (a::l)
    |b::l -> aux l
  in aux (inverse l);;


let tail b = match b with
  | []-> []
  | e::l -> l;;

let rec shift_n nA d = match (nA, d) with
  |([],d) -> []
  |(l, d) when d = 0 -> l
  |(l, d)-> (shift_n (0::l) (d-1));;

let rec mult_n nA nB reaminder = let res = match (nA, nB) with
    | _::_, []
    | [], _::_
    | [], [] -> []
    | lA, eB::lB ->
        if eB = 0
            then (mult_n lA lB (reaminder+1))
        else
            add_n (shift_n lA reaminder) (mult_n lA lB (reaminder+1))
    in res;;

let quot_b bA bB =
    let quot bA bB =
    if bB = []
        then failwith "quotb: division by zero"
    else
        let rec aux0 l1 l2 n remainder = match (l1,l2) with
            |(e1::l1, e2::l2) when e2 >>! n -> (tail (l1),(l2))
            |(e1::l1, l2) -> aux0 ((0::e1)::e1::l1) ((mult_n e1 remainder 0)::l2) n remainder
            |(_,_)  -> ([], [])
        in
    let a, b = aux0 [[1]] [] (tail bA) (tail bB) in
    let rec aux (l1,l2) q r = match (l1, l2) with
        |([],_) -> q
        |(_, []) -> q
        |(e::l, e1::l1) ->
            if add_n r e1 >>! tail bA
                then aux (l,l1) q r
            else
                aux (l, l1) (add_n e q) (add_n e1 r)
    in match (bA, bB) with
        |(e::l, e1::l1) when e = e1 -> 0::(add_n (aux (a, b) [] []) [])
        |(e::l, e1::l1) -> 1::(add_n (aux (a,b) [] []) [])
        |(_ , []) -> []
        |([], _) -> []
    in
    let bA = clear bA in
    let bB = clear bB in
    let q = clear (quot bA bB) in
    let s = if bB << [] then [1;1] else [0;1] in
    if diff_b bA (mult_b bB q) << []
        then diff_b q s
    else q;;



(** Modulo of a bitarray against a positive one.
    @param bA Bitarray the modulo of which you're computing.
    @param bB Bitarray which is modular base.
 *)
let mod_b bA bB =
  if (mult_b bB (quot_b bA bB)) = bA
  then []
  else
    (diff_b bA (mult_b (quot_b bA bB)bB));;

(** Integer division of two bitarrays.
    @param bA Bitarray you want to divide.
    @param bB Bitarray you wnat to divide by.
*)
let div_b bA bB =((quot_b bA bB),(mod_b bA bB));;

