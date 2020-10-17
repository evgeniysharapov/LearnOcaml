(* 
  Faktor problem
  https://open.kattis.com/problems/faktor

  b = round_up ( x / a )
  x = a * b
  
*)

let line = read_line() in
let ai = Str.split (Str.regexp " ") line in
let a = int_of_string (List.nth ai 0) in
let b = int_of_string (List.nth ai 1) in
let x = a *( b - 1 ) + 1 in
begin
  Printf.printf "%d\n" x  
end;  
