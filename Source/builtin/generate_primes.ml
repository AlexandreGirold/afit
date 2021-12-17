(** Generating primes *)

(**#use "test_primes.ml";;*)

(* List composed of 2 and then odd integers starting at 3.
    @param n limit of list of odd integers, minimum value is 2.*)
let init_eratosthenes n = let rec aux odd l =
                            match n mod 2 with
                            |0-> if (n-1)>=odd then odd::(aux (odd+2) l)
                                 else l
                            |_-> if n>=odd then  odd::(aux (odd+2) l)
                                 else l
                          in 2::(aux 3 []);;


let is_prime n =
  if n=2 then true
  else
    let rec prime n i =
                   if n mod i =0 then false
                   else if (i*i)>n then true
                   else prime n (i+1)
    in prime n 2;;


(** Eratosthene sieve.
    @param n limit of list of primes, starting at 2.
*)
let eratosthenes n = let rec aux prime l = match is_prime(prime)with
                       |true -> if prime<= n then prime::(aux (prime+1) l)
                                else l
                       |_-> aux (prime+1) l
                     in (aux 2 []);;


(** Write a list into a file. Element seperator is newline.
    @param file path to write to.
 *)

let write_list li file =
    let oc = open_out file in
    let rec write = function
        []   -> close_out oc
       |e::l -> Printf.fprintf oc "%d\n" e; write l
    in write li;;


(** Write a list of prime numbers up to limit into a txt file.
    @param n limit of prime numbers up to which to build up a list of primes.
    @param file path to write to.
*)
let write_list_primes n file = write_list (eratosthenes (n)) file;;


(** Read file safely ; catch End_of_file exception.
    @param in_c input channel.
 *)
let input_line_opt in_c =
  try Some (input_line in_c)
  with End_of_file -> None

(** Create a list out of reading a line per line channel.
    @param in_c input channel.
 *)
let create_list in_c =
  let rec _create_list in_c =
    match input_line_opt in_c with
    | Some line -> (int_of_string line)::(_create_list in_c)
    | None -> []
  in
  _create_list in_c

(** Load list of primes into OCaml environment.
    @param file path to load from.
 *)
let read_list_primes file = create_list(open_in file);;


(** Get biggest prime.
    @param l list of prime numbers.
 *)
let rec last_element l = match l with
  | [] -> failwith "You're list is empty. "
  | e::[] -> e
  | h::t -> last_element t

(** Get two biggest primes.
    @param l list of prime numbers.
 *)
let rec last_two l = match l with
  | [] | [_] -> failwith "List has to have at least two prime numbers."
  | e::g::[] -> (e, g)
  | h::t -> last_two t

(** Finding couples of primes where second entry is twice the first
    plus 1.
    @param limit positive integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)

let double_primes limit isprime =
  let rec aux p1 l =
    match isprime(p1)&&isprime(p1*2+1) with
    |true when p1<=limit -> (p1,p1*2+1)::(aux (p1+1)l)
    |false when p1<=limit-> aux (p1+1) l
    |_-> l
  in aux 2 [];;

double_primes 20 is_prime ;;


(** Finding twin primes.
    @param limit positive integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)
let twin_primes limit isprime =
   let rec aux p1 l =
    match isprime(p1)&&isprime(p1+2) with
    |true when p1<=limit -> (p1,p1+2)::(aux (p1+1)l)
    |false when p1<=limit-> aux (p1+1) l
    |_-> l
  in aux 2 [];;

