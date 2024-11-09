open Core
open Util

let p_line s =
  let splits = String.split s ~on:'-' in
  let (n, check) = match String.split ~on:'[' (List.last_exn splits) with
    | [n; check] -> (int_of_string n, String.drop_suffix check 1)
    | _          -> failwith "malformed input"
  in String.to_list @@ String.concat @@ List.drop_last_exn splits
   , n
   , check

let inp = List.map ~f:p_line (In_channel.read_lines "../inputs/day04.txt")

let valid_rooms xs =
  List.(filter_map xs ~f:(fun (a, b, c) ->
            let most_common =
              take (frequencies a) 5 |> map ~f:fst |> String.of_char_list |> sort_string
            in if String.(most_common = sort_string c)
               then Some (a, b)
               else None))

let part1 = List.fold (valid_rooms inp) ~init:0 ~f:(fun acc (_, b) -> acc + b)

let part2 =
  (* Only lower case letters, so we can get away with this. *)
  let rotate n c =
    let scale = Char.to_int 'a'
    in Char.of_int_exn @@ scale + (Char.to_int c - scale + n) mod 26
  in List.filter_map (valid_rooms inp) ~f:(fun (s, b) ->
         let real_name = String.of_char_list (List.map s ~f:(rotate b))
         in if String.is_substring real_name ~substring:"north"
            then Some b
            else None)

let day4 = (string_of_int part1, string_of_int (List.hd_exn part2))
