module Frac = struct
  type t = { nom: int; denom: int }

  let make n d = { nom = n; denom = d }
  
  let rec gcd x y = if y == 0 then x else gcd y ( x mod y )

  let reduce f =
    let g = gcd f.nom f.denom in
    { nom = f.nom / g; denom = f.denom / g }
    
  let add x y =
    reduce { nom = x.nom * y.denom  + y.nom * x.denom; denom = y.denom*x.denom }
              
end
