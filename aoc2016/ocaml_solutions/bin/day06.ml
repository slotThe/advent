open Core
open Ocaml_solutions.Util
open Ocaml_solutions.Util.Fun

let () =
  let letter_frequencies get =
    In_channel.read_lines "../inputs/day06.txt"
    |> List.map ~f:String.to_list |> List.transpose_exn
    |> List.map ~f:(fst @. get @. frequencies)
    |> String.of_char_list
  in
  print_answer 6
    (letter_frequencies List.hd_exn, letter_frequencies List.last_exn)
