open Core
open Util
open Util.Fun

module ISet = Set.Make(Int)

let mk_assoc_array xs =
  let p_input inp =
    let inp = (String.split inp ~on:'|')
    and p_integers xs = List.filter_map ~f:int_of_string_opt
                          (String.split xs ~on:' ') in
    List.reduce_exn ~f:Set.inter
      (List.map ~f:(p_integers >> ISet.of_list) inp)
    |> Set.length
  in
  List.(range 1 (length xs) ~stop:`inclusive
        |> map ~f:(fun n -> (1, p_input (nth_exn xs (n - 1))))
        |> Array.of_list)

let part1 arr =
  Array.map arr
    ~f:(fun (_, n) -> Float.to_int (2. ** (float_of_int n -. 1.)))
  |> asum

let part2 map =
  for i = 0 to Array.length map - 1 do   (* End is inclusive by default! *)
    let (mult, correct_guesses) = map.(i) in
    for win = i + 1 to i + correct_guesses do
      map.(win) <- (fst map.(win) + mult, snd map.(win)) (* Mutation boiiis *)
    done
  done;
  Array.map ~f:fst map |> asum

let day04 =
  let inp = mk_assoc_array @@ In_channel.read_lines "../../inputs/day04.txt"
  in string_of_int (part1 inp)
   , string_of_int (part2 inp)
