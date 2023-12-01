open Core
open Util

let replacements1 =
  List.(map (range 1 9 ~stop:`inclusive)
          ~f:(fun n -> (string_of_int n, n)))

let replacements2 =
  replacements1 @ [ ("one",   1)
                  ; ("two",   2)
                  ; ("three", 3)
                  ; ("four",  4)
                  ; ("five",  5)
                  ; ("six",   6)
                  ; ("seven", 7)
                  ; ("eight", 8)
                  ; ("nine",  9)
                  ]

let solve (repls : (string * int) list) inp =
  let find_number isIn xs =
    let rec go = function
      | []        -> failwith "impossible"
      | (s :: ss) ->
         match List.find repls ~f:(fun (a, _) -> isIn a s) with
         | None        -> go ss
         | Some (_, b) -> b
    in go xs in
  let first = find_number
                (fun prefix b -> String.is_prefix ~prefix b)
                (String.tails inp)
  and second = find_number
                 (fun suffix b -> String.is_suffix ~suffix b)
                 (List.rev (String.inits inp))
  in first * 10 + second

let day01 =
  let inp = In_channel.read_lines "../../inputs/day01.txt" in
  let find_nums repls = string_of_int @@ sum @@
                          List.map inp ~f:(fun s -> solve repls s)
  in find_nums replacements1
   , find_nums replacements2
