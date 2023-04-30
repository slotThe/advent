open Core
open Ocaml_solutions

let print_answer n (one, two) =
  let () = print_endline ""
  and () = print_endline (String.concat ~sep:" " ["!!! DAY"; string_of_int n; "!!!"])
  and () = print_endline (String.concat ~sep:" " ["Part 1:"; one])
  and () = print_endline (String.concat ~sep:" " ["Part 2:"; two])
  in ()

let () = print_answer 1 Day1.day1
let () = print_answer 2 Day2.day2
let () = print_answer 3 Day3.day3
