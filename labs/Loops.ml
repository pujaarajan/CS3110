(*Exercise: modify your implementation of normalize to use one of these looping expressions.*)

(*Exercise: use List.iter and print_endline to print a list of strings.*)

let print_string_list l =
    List.iter (print_endline x) l

print_string_list ["a";"b";"c"]

(*WHY DOESN'T IT PRINT ANYTHING*)