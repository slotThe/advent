open Core

let split : on:string -> string list -> string list list =
 fun ~on xs ->
  let rec go (r :: rs) = function
    | [] -> List.rev (r :: rs)
    | x :: xs ->
        if equal_string x on then go ([] :: r :: rs) xs
        else go ((x :: r) :: rs) xs
  in
  go [ [] ] xs

let day01 : int list =
  In_channel.read_lines "../../inputs/day1.txt"
  |> split ~on:""
  |> List.map ~f:(fun l ->
         List.map ~f:int_of_string l |> List.fold ~init:0 ~f:( + ))

let part1 : int =
  List.sort day01 ~compare:compare_int |> List.rev |> List.hd_exn

let part2 : int =
  List.sort day01 ~compare:compare_int |> List.rev |> fun l ->
  List.take l 3 |> List.fold ~init:0 ~f:( + )
