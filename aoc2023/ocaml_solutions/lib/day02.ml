open Core
open Util
open Util.Parse

let possible (n, t) =
  n <= match t with
       | "red" -> 12
       | "green" -> 13
       | "blue" -> 14
       | _ -> -1

let parse =
  let mk_cols = List.map ~f:(fun ms -> (int_of_string ms.(1), ms.(2)))
  in In_channel.read_lines "../../inputs/day02.txt"
     |> List.map ~f:(re_all "(\\d+) (red|green|blue)")
     |> List.map ~f:mk_cols

let part1 =
  parse
  |> List.filter_mapi
       ~f:(fun n g -> if List.for_all ~f:possible g
                      then Some (n + 1)
                      else None)
  |> sum

let part2 =
  let max_in_group group =
    List.map group ~f:fst
    |> List.fold ~init:0 ~f:max
  in
  parse
  |> List.map
       ~f:(fun l -> List.sort_and_group l
                      ~compare:(fun (_, a) (_, b) -> compare_string a b)
                    |> List.map ~f:max_in_group
                    |> List.fold ~init:1 ~f:( * ))
  |> sum

let day02 = string_of_int part1, string_of_int part2
