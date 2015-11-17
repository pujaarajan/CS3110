type vector = float array

let normalize (v : vector) : unit =
  let enorm = sqrt((fun acc x -> x^2 + acc) 0 v) in
    map(fun x -> x/enorm) v

(*MAP CREATES A NEW ARRAY HOW DO I DO IT IN PLACE*)
