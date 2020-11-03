(*
 Solution for https://open.kattis.com/problems/kleptography
 *)

(* Returns character t encoded using character k *)
let from_num n =
  let open Char in
  chr ( n + code 'a')

let to_num c =
  let open Char in
  code c - code 'a'

let enc k t =
  from_num ((to_num t + to_num k) mod 26)

(* decodes character decoded from c using key k *)
let dec k c =
  from_num ((26  + to_num c - to_num k) mod 26)

(* Encrypt text using key. Key has to be the same length or longer than text *)
let shift_cipher key text =
  let encode i = enc (key.[i]) in
  String.mapi encode text

let shift_cipher_reverse key cipher =
  let decode i = dec (key.[i]) in
  String.mapi decode cipher

let autokey_cipher key text =
  shift_cipher (key^text) text

(* cipher is the message, peeked is last part of plain text that we saw *)
let autokey_break cipher peeked =
  let open String in
  let plaintextLength = length cipher in
  let plaintext = ref peeked in
  while length !plaintext <= length cipher do
    let len = length (!plaintext) in
    let start = plaintextLength - len in
    let subcipher = sub cipher start len in
    let key = shift_cipher_reverse (!plaintext) subcipher in
    plaintext := key ^ peeked
  done;
  (* now plaintext has some of the prefix and the plaintext we want just plaintext *)
  sub !plaintext ((length !plaintext) - plaintextLength) plaintextLength
  

;;
(* reading intput  *)
let line = read_line() in
let [@warning "-8"] n::m::[] = List.map int_of_string (Str.split (Str.regexp " ") line ) in
let last_nplain = read_line() in
let ciphertext = read_line() in
let ans = autokey_break ciphertext last_nplain in
Printf.printf "%s\n" ans
