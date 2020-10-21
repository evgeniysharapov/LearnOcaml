open OUnit2;;    
open Cipher;;

(* One could run tests by 

   $ ocamlfind ocamlc -o test -package oUnit -linkpkg -g cipher.ml cipher_test.ml
   $ ./test

 *)
let test_enc _ =
  assert_equal 'd' (enc 't' 'k');
    
let suite =
"suite">:::
 ["test_enc">:: test_enc;
  "test_dec">:: test_enc]
;;

let () =
  run_test_tt_main suite   
;;
