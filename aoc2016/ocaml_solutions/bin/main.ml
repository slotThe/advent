open Core
open Ocaml_solutions

let print_answer n (one, two) =
  print_endline "";
  print_endline (String.concat ~sep:" " ["!!! DAY"; string_of_int n; "!!!"]);
  print_endline (String.concat ~sep:" " ["Part 1:"; one]);
  print_endline (String.concat ~sep:" " ["Part 2:"; two]);
  ()

let () =
  print_answer 1 Day1.day1;
  print_answer 2 Day2.day2;
  print_answer 3 Day3.day3;
  print_answer 4 Day4.day4;
  (* print_answer 5 Day5.day5; *)
  print_answer 6 Day6.day6;
  print_answer 7 Day7.day7;
  print_answer 8 Day8.day8;
  print_answer 9 Day9.day9;
  print_answer 10 Day10.day10;
  print_answer 12 Day12.day12;
  ()
