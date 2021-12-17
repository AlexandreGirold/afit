(** Ciphers
    Builtin integer based ciphers.*)
(*
#use "builtin.ml";;
#use "generate_primes.ml";;
#use "power.ml";;
 *)

open Builtin;;
open Power;;

let bezout a b =
  let rec aux a b =
    if a=0 then (0,1,b)
    else
      let q,r =div b a  in
      let (v,d,u) = aux r a in
      (d-q*v,v,u)
  in aux a b;;


(********** Cesar Cipher **********)

(** Cesar's cipher encryption
    @param k is an integer corresponding to key
    @param m word to cipher.
    @param b base ; for ASCII codes should be set to 256.
 *)
let encrypt_cesar k m b =
  if m=[]
  then invalid_arg "m must not be empty"
  else
    let rec aux k m l2 =
      if k>b
      then aux (k mod b) m l2
      else
        match m with
        |e::[] -> if e+k<b && e+k>=0
                           then ((e+k) mod b)::l2
                           else
                             if e+k<0 then (e+k+b)::l2
                             else (e+k)::l2
        |e::l when e+k<=b -> if e+k<0
               then (e+k+b)::(aux k l l2)
               else
                (e+k)::(aux k l l2)
        |e::l-> ((e+k) mod b)::(aux k l l2)
        |_-> invalid_arg "test"

    in aux k m [];;

(** Cesar's cipher decryption
    @param k is an integer corresponding to key
    @param m encrypted word.
    @param b base ; for ASCII code should be set to 256.
 *)

let decrypt_cesar k m b =
  encrypt_cesar (-k) m b;;


(********** RSA Cipher **********)

(** Generate an RSA ciphering keys.
    Involved prime numbers need to be distinct. Output is a couple
    of public, private keys.
    @param p prime number
    @param q prime number
 *)

let generate_keys_rsa p q =
  let n = p*q in
  let fi= (p-1)*(q-1) in
  let e = fi-1 in
  let (d,_,_) = bezout e fi in
  if p=q then invalid_arg "p and q must be different prime numbers"
  else
    ((n,e),(n,d));;
generate_keys_rsa 9967 9973

(** Encryption using RSA cryptosystem.
    @param m integer hash of message
    @param pub_key a tuple (n, e) composing public key of RSA cryptosystem.
 *)
let encrypt_rsa m (n, e) = prime_mod_power m e n;;

(** Decryption using RSA cryptosystem.
    @param m integer hash of encrypter message.
    @param pub_key a tuple (n, d) composing private key of RSA cryptosystem.
 *)
 let decrypt_rsa m (n , d) = prime_mod_power m d n;;


(********** ElGamal Cipher **********)

(** Generate ElGamal public data. Generates a couple (g, p)
    where p is prime and g primitive root in F_p.
    @param p is prime having form 2*q + 1 for prime q.
 *)
let rec public_data_g p = (0, 0)

(** Generate ElGamal public and private keys.
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let generate_keys_g (g, p) = (0, 0)

(** ElGamal encryption process.
    @param msg message to be encrypted.
    @param pub_data a tuple (g, p) of ElGamal public data.
    @param kA ElGamal public key.
 *)
let encrypt_g msg (g, p) kA = (0, 0)

(** ElGamal decryption process.
    @param msg a tuple (msgA, msgB) forming an encrypted ElGamal message.
    @param a private key
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let decrypt_g (msgA, msgB) a (g, p) = 0
