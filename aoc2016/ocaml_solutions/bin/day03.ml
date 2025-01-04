open Core
open Ocaml_solutions.Util

let inp =
  let p_int s n = int_of_string String.(sub s ~pos:n ~len:5 |> strip) in
  In_channel.read_lines "../inputs/day03.txt"
  |> List.map ~f:(fun s -> (p_int s 0, p_int s 5, p_int s 10))

let could_be_triangle xs =
  let check (a, b, c) = a + b > c && a + c > b && b + c > a in
  List.length (List.filter xs ~f:check)

let part2 =
  let tuple_to_list (a, b, c) = [ a; b; c ] in
  let rec list_to_tuple = function
    | a :: b :: c :: xs -> (a, b, c) :: list_to_tuple xs
    | [] | _ -> []
  in
  could_be_triangle
    List.(
      map ~f:tuple_to_list inp |> transpose_exn |> concat_map ~f:list_to_tuple)

let () =
  print_answer 3 (string_of_int (could_be_triangle inp), string_of_int part2)
