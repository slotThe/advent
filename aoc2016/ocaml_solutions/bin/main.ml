let print_answer n (one, two) =
  let () = print_endline ""
  and () = print_endline (String.concat " " ["!!! DAY"; string_of_int n; "!!!"])
  and () = print_endline (String.concat " " ["Part 1:"; one])
  and () = print_endline (String.concat " " ["Part 2:"; two])
  in ()

let () = print_answer 1 Ocaml_solutions.Day1.day1
let () = print_answer 2 Ocaml_solutions.Day2.day2
