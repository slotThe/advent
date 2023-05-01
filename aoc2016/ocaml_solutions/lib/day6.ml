open Core
open Util.Fun

let day6 =
  let letter_frequencies get =
    In_channel.read_lines "../../inputs/day06.txt"
    |> List.map ~f:String.to_list
    |> List.transpose_exn
    |> List.map ~f:(fst @. get @. Util.frequencies)
    |> String.of_char_list
  in letter_frequencies List.hd_exn
   , letter_frequencies List.last_exn
