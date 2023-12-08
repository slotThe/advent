open Core
open Ocaml_solutions


let print_answer n (one, two) =
  print_endline "";
  print_endline (String.concat ~sep:" " ["!!! DAY"; string_of_int n; "!!!"]);
  print_endline (String.concat ~sep:" " ["Part 1:"; one]);
  print_endline (String.concat ~sep:" " ["Part 2:"; two]);
  ()

let () =
  print_answer 1 Day01.day01;
  print_answer 4 Day04.day04;
  print_answer 6 Day06.day06;
  print_answer 8 Day08.day08;
  ()
