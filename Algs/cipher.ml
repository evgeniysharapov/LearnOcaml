(* Different ciphers *)
(* 
 Shift ciphers
 based on https://www.khanacademy.org/computing/computer-science/cryptography/ciphers/a/shift-cipher
 *)

(* Returns character t encoded using character k *)
let enc k t =
  let open Char in
  let to_num c = code c - code 'a' in
  let from_num i = chr ( i + code 'a') in
  from_num ((to_num t + to_num k) mod 26);;

(* decodes character decoded from c using key k *)
let dec k c =
  let open Char in
  let to_num h = code h - code 'a' in
  let from_num i = chr ( i + code 'a') in
  from_num ((26  + to_num c - to_num k) mod 26);;

(* Encrypt text using key. Key has to be the same length or longer than text *)
let shift_cipher key text =
  let encode i = enc (key.[i]) in
  String.mapi encode text;;

let shift_cipher_reverse key cipher =
  let decode i = dec (key.[i]) in
  String.mapi decode cipher;;

      
