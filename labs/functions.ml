let cube x = x**3.

let cube_root x = x**float(1/3)

let area r = let pi = acos (-1.) in pi*.r**2.

let rec fact x = if x <= 1 then 1 else x * fact (x - 1)

let rec fib x = if x <= 1 then 1 else fib (x - 1) + fib (x - 2)

let addn n : int -> int = fun x -> x + n

let add x y = x+y

let add10 = add 10

let ($$) x y = (x +. y)/.2

let rec n_times f n x = if n = 0 then x else n_times f (n-1) (f x)

let divide ~numerator ~denominator = numerator/.denominator

let rev1_helper n =
  let rec loop acc = function
    | 0 -> acc
    | n -> loop (acc * 10 + n mod 10) (n / 10) in
  loop 0 n

let rev1_int n = let o = rev_helper n in if o > n then n else o

let rev2_base ?base:(b=10) n = let o = rev_helper b n in if o > n then n else o

let rev2_helper ?base:(base=10) n =
  let rec loop acc = function
    | 0 -> Printf.printf "rec"; acc
    | n -> Printf.printf "rec"; loop (acc * b + n mod b) (n / b) in
  loop 0 n

