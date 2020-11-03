(* Different ciphers *)
(* 
 Shift ciphers
 based on https://www.khanacademy.org/computing/computer-science/cryptography/ciphers/a/shift-cipher
 *)

(* Returns character t encoded using character k *)
let from_num n =
  let open Char in
  chr ( n + code 'a')

let to_num c =
  let open Char in
  code c - code 'a'

let enc k t =
  from_num ((to_num t + to_num k) mod 26);;

(* decodes character decoded from c using key k *)
let dec k c =
  from_num ((26  + to_num c - to_num k) mod 26);;

(* Encrypt text using key. Key has to be the same length or longer than text *)
let shift_cipher key text =
  let encode i = enc (key.[i]) in
  String.mapi encode text;;

let shift_cipher_reverse key cipher =
  let decode i = dec (key.[i]) in
  String.mapi decode cipher;;

let autokey_cipher key text =
  shift_cipher (key^text) text;;

(* cipher is the message, peeked is last part of plain text that we saw *)
let autokey_break cipher peeked =
  let len = String.length peeked in
  let start = String.length cipher - len in
  let subcipher = String.sub cipher start len in
  let key = shift_cipher_reverse peeked subcipher in
  (* now we can use the key to decipher rest of the message *)
  key;;
