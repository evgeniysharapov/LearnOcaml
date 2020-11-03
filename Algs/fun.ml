(* 
   Some functional paradigms conspicuosly missing from standard Ocaml
*)


(* Dual of foldr given a generative function and seed will generate list
   Note, that we generate list, but would be nice to parametrize it
*)
let rec unfoldr f s =
  match f s with
  | None -> []
  | Some (a, snext ) -> a :: unfoldr f snext
;;
(* Returns list ( again would be nice to parametrize) of integers from l to r (excl.) *)
let range l r =
  unfoldr (fun x -> if x >= r then None else Some (x,x+1)) l
